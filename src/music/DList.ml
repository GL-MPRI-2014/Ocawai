(**
   DList module
*)

open Music
open Format

type time = Time.t
type event = Music.event

let (/+/) : time -> time -> time = fun x y -> (Time.plus x y)
let (/-/) : time -> time -> time = fun x y -> (Time.minus x y)

(** Tag manipulation functions *)

type tag = Tag of (time (** Value dur *)
		   * (time option) (** Value start*)
)

(** Base DLists *)

type t = Sync of time
	 | Event of event
	 | Prod of (tag * t * t)
	 | Modify of (Modify.t * t)

(**
   Modifies the input [t] and adds the input modifier
 *)
let modify : Modify.t -> t -> t = fun modifier t ->
  Modify (modifier, t)

let rec getTag : t -> tag = function
  | Sync time -> Tag (time, None)
  | Event event -> Tag (Time.zero, Some Time.zero)
  | Prod (tag, _, _) -> tag
  | Modify (_, t) -> getTag t

let tagProd : tag -> tag -> tag =
  fun (Tag (dur1, start1)) (Tag (dur2, start2)) -> 
    let newStart = match (start1, start2) with
      | (None, None) -> None
      | (None, Some st_time) -> Some (st_time /+/ dur1)
      | (Some st_time, None) -> Some st_time
      | (Some st_time1, Some st_time2) -> Some (Time.min st_time1 (st_time2 /+/ dur1))
    in Tag (dur1 /+/ dur2, newStart)

let sync : time -> t = fun time -> Sync time

let zero : t = sync Time.zero

let isZero : t -> bool = fun t -> t = zero

let isSync : t -> bool = fun t ->
  let Tag(_, start) = getTag t in
  match start with
  | Some _ -> false
  | None -> true

let return : Music.event -> t = fun event -> Event event

let duration : t -> Time.t = fun t ->
  let Tag(dur, _) = getTag t in
  dur

(** DList manipulation functions *)

let (/::/) : t -> t -> t = fun t1 t2 ->
  if isZero t1 then t2
  else
    if isZero t2 then t1
    else
      begin
	let Tag (dur, start) = tagProd (getTag t1) (getTag t2) in
	if start = None then
	  sync dur
	else Prod(Tag(dur, start), t1, t2)
      end

let returnWithDelay : Music.event -> t = fun event ->
  let dur = Music.duration event in
  (return event) /::/ (sync dur)  

(** List <-> DList functions *)

let fromList_parallel : Music.event list -> t =
  List.fold_left
    (fun acc event -> acc /::/ return event) zero

let fromList_sequence : Music.event list -> t =
  List.fold_left
    (fun acc event -> acc /::/ returnWithDelay event) zero

(** {2 Normalization utilities} *)

module MusicContextT =
  struct
    type t = event * Modify.Context.t
    let compare = fun (e1, c1) (e2, c2) ->
      match Music.compare e1 e2 with
      | 0 -> Modify.Context.compare c1 c2
      | comp -> comp
  end
    
module MusicContextSet = Set.Make(MusicContextT)

type headTail = {mutable to_events : time;
		  (* The distance from the head's Pre to
		     the events it holds. *)
		 mutable events : MusicContextSet.t;
		 mutable to_next : time;
		 (* The delay from the head's events to the first next events in
                    the tile OR <when the tail holds no more events> to Pos *) 
		 mutable tailT : t
		}

let makeHeadTail to_events events to_next tail =
  {to_events = to_events; events = events; to_next = to_next; tailT = tail}

(** {2 Printing functions} *)

let rec fprintf : Format.formatter -> t -> unit = fun fmt -> function
  | Event event -> Format.fprintf fmt "@[<1>Event(%a@,)@]@." Music.fprintf event
  | Sync dur -> Format.fprintf fmt "@[<1>Sync(%a@,)@]@." Time.fprintf dur
  | Prod(tag, t1, t2) ->
    Format.fprintf fmt "@[Prod(@[%a,@ %a,@ %a@]@,)@]@." fprint_tag tag fprintf_sub t1 fprintf_sub t2
  | Modify(modifier, t) ->
     Format.fprintf fmt "@[Modify(@[%a,@ %a@]@,)@]@." Modify.fprintf modifier fprintf_sub t

and fprintf_sub : Format.formatter -> t -> unit = fun fmt -> function
  | Event event -> Format.fprintf fmt "@[<1>Event(%a@,)@]" Music.fprintf event
  | Sync dur -> Format.fprintf fmt "@[<1>Sync(%a@,)@]" Time.fprintf dur
  | Prod(tag, t1, t2) ->
    Format.fprintf fmt "@[Prod(@[%a,@ %a,@ %a@]@,)@]" fprint_tag tag fprintf_sub t1 fprintf_sub t2
  | Modify(modifier, t) ->
     Format.fprintf fmt "@[Modify(@[%a,@ %a@]@,)@]" Modify.fprintf modifier fprintf_sub t

and fprint_tag fmt = function
  | Tag(dur, start) -> Format.fprintf fmt "@[<1>Tag(@[Dur =@ %a,@ Start =@ %a@]@,)@]"
    Time.fprintf dur fprint_timeOption start

and fprint_timeOption fmt = function
  | None -> Format.fprintf fmt "%s" "None"
  | Some(dur) -> Format.fprintf fmt "@[%a@]" Time.fprintf dur

and fprint_headTail fmt = fun ht ->
  let to_events = ht.to_events
  and events_set = ht.events
  and to_next = ht.to_next
  and tailT = ht.tailT in
  Format.fprintf fmt "@[<1>headTail(@[to_events =@ %a,@ \
		      events =@ %a,@ to_next =@ %a,@ tailT =@ %a@]@,)@]"
		 Time.fprintf to_events fprint_eventsSet events_set Time.fprintf to_next
		 fprintf_sub tailT
		 
and fprint_eventsSet fmt = fun set ->
  let rec pp_print_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
    (* Copied from the 4.02.0 stdlib, formats a list *)
    | [] -> ()
    | [v] -> pp_v ppf v
    | v :: vs ->
       pp_v ppf v;
       pp_sep ppf ();
       pp_print_list ~pp_sep pp_v ppf vs
  in
  let printer fmt eventContextList =
    (* To-Do : add fprintf for contexts and fix this function *)
    pp_print_list Music.fprintf fmt (List.map (fun (x, _) -> x) eventContextList) in
  Format.fprintf fmt "@[<1>MusicContextSet(@[%a@]@,)@]" printer (MusicContextSet.elements set) 

let printf : t -> unit = fprintf Format.std_formatter

(** {2 Normalization functions} *)

let make_with_context : t -> Modify.Context.t -> t = fun dlist context ->
  let f = fun dlist modifier -> Modify (modifier, dlist) in
  Modify.fold_left f dlist context
			
let headTail_tuple : t -> headTail = fun t ->
  (** Returns a tuple containing a decomposition of
      the input tail where :
      the "head" holds the first 'real' events of
      the tile (ie. no syncing), the tail
      contains the remaining events.
      to_events and to_next are the syncing plumbing
      needed to rebuild the tile correctly.
      This in an auxiliary function with the plumbing exposed.
   
      To-Do : Fix : Copying all contexts introduces an exponential space overhead.
      -> Is it really necessary ? 
   *)
  let rec aux_context : Modify.Context.t -> t -> headTail = fun context ->
    let add_context_tail : headTail -> headTail = fun ht ->
      let new_tail = make_with_context ht.tailT context in
      ht.tailT <- new_tail; ht
    in 
    function
    | Sync dur ->
       makeHeadTail Time.zero (MusicContextSet.empty) dur zero
    | Event event ->
       makeHeadTail Time.zero (MusicContextSet.singleton (event, context)) Time.zero zero
    | Modify (modifier, t) ->
       let new_context = let copy = Modify.Context.copy context in
			 Modify.replaceContext modifier copy; copy
       in 
       aux_context new_context t
    | Prod ((Tag (dur, start)), t1, t2) ->
       let Tag(dur1, startT1) = getTag t1
       and Tag(dur2, startT2) = getTag t2 in
       match (startT1, startT2) with
       | (None, None) ->
	  (** Both factors are pure sync, return the total sync. *)
	  aux_context (Modify.Context.copy context) (sync dur)
       | (None, Some _) ->
	  (** t1 is pure sync, shift t2. *)
	  let headTuple2 = aux_context (Modify.Context.copy context) t2
	  in (headTuple2.to_events <- (dur1 /+/ headTuple2.to_events);
	      add_context_tail headTuple2)
       | (Some _, None) ->
	  (** t2 is pure sync. *)
	  let headTuple1 = aux_context (Modify.Context.copy context) t1 in
	  let Tag(dur_tail1, start_tail1) = getTag headTuple1.tailT in
	  begin
	    (* Some new stuff here *)
	    match start_tail1 with
	    | Some(_) -> 
	       if (Time.sign (headTuple1.to_next) <= 0)
	       then (** There are no more events in t1's tail and t2 holds no events :
        	     the tail is pure sync. *)
		 ( headTuple1.to_next <- dur2;
		   add_context_tail headTuple1 )
	       else ( headTuple1.tailT <- (headTuple1.tailT) /::/ (sync dur2);
		      add_context_tail headTuple1 )
	    | None -> (** There are no more real events in t1's tail, it is pure sync,
                       of duration dur_tail1, let's mash everything up *)
	       ( headTuple1.to_next <- dur;
		 add_context_tail headTuple1 )
	  end
	    
       | (Some start1, Some start2) ->
	  let shifted_start2 = dur1 /+/ start2 in
	  match (let comp = Time.compare start1 shifted_start2 in
		 (comp < 0, comp > 0)) with
	  | (true, false) -> (* t1 starts first *)
	     let headTuple1 = aux_context (Modify.Context.copy context) t1 in
	     let next_of_head = start1 /+/ headTuple1.to_next in
	     begin
	       match ((Time.compare next_of_head shifted_start2) <= 0,
		      not (isSync headTuple1.tailT)) with
	       | true, true ->
		  (** t1's tail still holds some events, and they happen
                  before the start of t2 *)
		  begin
		    headTuple1.tailT <- headTuple1.tailT /::/ t2;
		    add_context_tail headTuple1
		  end
	       | false, true -> 
		  (** t1's tail still holds some events, but they happen after
                  t2's first events *)
		  begin
		    headTuple1.to_next <- shifted_start2 /-/ start1;
		    headTuple1.tailT <- (sync (next_of_head /-/ shifted_start2))
					/::/ headTuple1.tailT /::/ t2;
		    add_context_tail headTuple1
		  end
	       | _, false ->
		  (** t1's tail no longer holds events, it is pure sync
	          Let's shift to start of t2 and play *)
		  begin
		    headTuple1.to_next <- (Time.inverse start1) /+/ shifted_start2;
		    headTuple1.tailT <- t2;
		    add_context_tail headTuple1
		  end
	     end	  
	       
	  | (false, false) -> (* The first events in both t1 and t2 are synchronized *)
	     let headTuple1 = aux_context (Modify.Context.copy context) t1
	     and headTuple2 = aux_context (Modify.Context.copy context) t2 in
	     let tail1 = headTuple1.tailT
	     and tail2 = headTuple2.tailT in
	     let sync_endTail1ToNext2 =
	       (Time.inverse dur1) /+/ headTuple1.to_events /+/ headTuple2.to_next in
	     let newHT =
	       makeHeadTail headTuple1.to_events
			    (MusicContextSet.union headTuple1.events headTuple2.events)
			    headTuple1.to_next
			    (headTuple1.tailT /::/ (sync sync_endTail1ToNext2)
			     /::/ headTuple2.tailT)
	     in begin
	       match ((Time.compare headTuple2.to_next headTuple1.to_next) < 0,
		      not @@ isSync tail1, not @@ isSync tail2) with
	       | true, true, true ->
		  (** Both tiles still hold events, and those in tail2 come first *)
		  ( newHT.to_next <- headTuple2.to_next;
		    newHT.tailT <- (sync (headTuple1.to_next /-/ headTuple2.to_next)) /::/
				     headTuple1.tailT /::/
				       (sync sync_endTail1ToNext2) /::/ headTuple2.tailT;
		    add_context_tail newHT)
	       | false, true, true ->
		  (** Both tiles still hold events, and those in tail1 come first *)
		  ( add_context_tail newHT)
	       | _, false, true ->
		  (** Only tail2 still holds events, let's compress tail1
                    and skip to tail2 *)
		  newHT.to_next <- (Time.inverse start1) /+/ dur1 /+/
				     start2 /+/ headTuple2.to_next;
		  newHT.tailT <- tail2;
		  add_context_tail newHT
	       | _, true, false ->
		  (** Only tail1 still holds events, let's play tail1
                    and skip to Pos *)
		  newHT.tailT <- tail1 /::/ sync dur2;
		  add_context_tail newHT
	       | _, false, false ->
		  (** Both tails are pure sync, let's compress it all ! *)
		  newHT.to_next <- dur /-/ start1;
		  newHT.tailT <- zero;
		  add_context_tail newHT
	     end
		  
	  | (false, true) -> (* t2 starts first *)
	     let headTuple1 = aux_context (Modify.Context.copy context) t1 in
	     let headTuple2 = aux_context (Modify.Context.copy context) t2 in
	     let next_of_head = shifted_start2 /+/ headTuple2.to_next in
	     let newHT = makeHeadTail shifted_start2 headTuple2.events in
	     begin
	       match (Time.compare next_of_head start1 <= 0, not (isSync headTuple2.tailT)) with
	       | true, true ->
		  (** t2's tail still holds events, and they start before t1 *) 
		  begin
		    (** The next events are in t2's tail *) 
		    add_context_tail @@newHT headTuple2.to_next @@
		      (sync @@ Time.inverse next_of_head) /::/
			t1 /::/
			  (sync @@ headTuple2.to_events /+/ headTuple2.to_next) /::/
			    headTuple2.tailT
		  end
	       | false, true ->
		  (** t2's tail still holds events, BUT they start AFTER t1 *)
		  begin
		    add_context_tail @@
		      newHT ((Time.inverse shifted_start2) /+/ headTuple1.to_events) @@
		      (sync @@ Time.inverse @@ headTuple1.to_events) /::/ t1 /::/
			(sync @@ headTuple2.to_events /+/ headTuple2.to_next) /::/
			  headTuple2.tailT
		  end
	       | _, false ->
		  (** t2's tail no longer holds events, it is pure sync
		    Let's shift back to pre, then to start of t1 and play *)
		  begin
		    add_context_tail @@
		      newHT ((Time.inverse shifted_start2) /+/ headTuple1.to_events) @@
		      (sync @@ Time.inverse @@ headTuple1.to_events) /::/ t1 /::/
			(sync @@ headTuple2.to_events /+/ headTuple2.to_next) /::/
			  headTuple2.tailT
		  end
		    
	     end
	  | _ -> assert false
  in
  aux_context (Modify.Context.empty ()) t
	
let make_with_context : event -> Modify.Context.t -> t = fun event context ->
  let f = fun dlist modifier -> Modify (modifier, dlist) in
  Modify.fold_left f (return event) context

let from_contextList_parallel : (event * Modify.Context.t) list -> t =
  fun eventContext_list ->
  let make_one = fun (event, context) -> make_with_context event context in
  List.fold_left (/::/) zero @@ List.map make_one eventContext_list
		
let headTail_decomp : t -> t * t * t * t =
  fun t -> 
  let headTailT = headTail_tuple t in
  let to_events = sync (headTailT.to_events)
  and events = from_contextList_parallel (MusicContextSet.elements headTailT.events)
  and to_next = sync headTailT.to_next in
  (to_events, events, to_next, headTailT.tailT)

let headTail : t -> t * t =
  (* Return the head and the tail of the input tile, with all the plumbing
     applied and hidden. *)
  fun t ->
    let headTailT = headTail_tuple t in
    let head =
      ((sync (headTailT.to_events)) /::/ from_contextList_parallel 
					   (MusicContextSet.elements headTailT.events)) /::/
	(sync headTailT.to_next)
    in (head, headTailT.tailT)

let normalize_new : t -> t = fun t -> 
  let rec aux acc = fun t ->
    let (to_events, events, to_next, tail) = headTail_decomp t in
    if isZero tail then
      ((acc /::/ to_events) /::/ events) /::/ to_next
    else aux (((acc /::/ to_events) /::/ events) /::/ to_next) tail
  in aux zero t

let rec normalize_old : t -> t = fun t ->
  let (head, tail) = headTail t in
  if isZero tail then
    head
  else head /::/ normalize_old tail

let normalize = normalize_new

(** Equality function *)

let rec is_silent : t -> bool = function
  | Sync _ -> true
  | Event (event) -> Music.is_silent event
  | Prod (tag, t1, t2) -> is_silent t1 && is_silent t2
  | Modify (_, t) -> is_silent t

(** Tag syntactic equality *)
let is_equal_tag : tag -> tag -> bool = fun tag1 tag2 ->
  match (tag1, tag2) with
  | Tag (dur1, None), Tag (dur2, None) -> Time.is_equal dur1 dur2
  | Tag (dur1, Some(start1)), Tag (dur2, Some(start2)) ->
     Time.is_equal dur1 dur2 &&
       Time.is_equal start1 start2
  | _ -> false

(** Syntactic equality *)
let rec is_equal : t -> t -> bool = fun t1 t2 ->
  match (t1, t2) with
  | Sync dur1, Sync dur2 -> Time.is_equal dur1 dur2
  | Event event1, Event event2 -> Music.is_equal event1 event2
  | (Prod(tag1, t1_1, t1_2), Prod(tag2, t2_1, t2_2)) ->
     is_equal_tag tag1 tag2 &&
       is_equal t1_1 t2_1 &&
	 is_equal t1_2 t2_2
  | _ -> false
    
(** {2 MIDI conversion} *)

(**
   Converts a normalized DList to a [MIDI.Multitrack.buffer] (respecting the mapping
   of the instruments

   Semantics : the [MIDI.Multitrack.buffer]'s beginning is the first event in the DList
 *)
let rec toMidi : ?channels:int -> ?samplerate:int -> ?division:MIDI.division ->
		 ?tempo:Time.Tempo.t -> ?context:Modify.Context.t ->
		 t -> MIDI.Multitrack.buffer option =
  fun ?channels:(channels = MidiV.channels) ?samplerate:(samplerate = MidiV.samplerate)
      ?division:(division = MidiV.division) ?tempo:(tempo = Time.Tempo.base)
      ?context:(context = Modify.Context.empty ()) ->
  let local_musicToMidi : Music.event -> MIDI.Multitrack.buffer =
    Music.toMidi ~samplerate ~division ~tempo ~context
  in 
  function
  | Event event -> Some(local_musicToMidi event)
  | Sync dur -> None
  | Prod (Tag(dur, _), t1, t2) (* as t *) ->
     begin
       let local_DLtoMidi : t -> MIDI.Multitrack.buffer option =
	 toMidi ~samplerate ~division ~tempo ~context in
       let b1_opt = local_DLtoMidi t1 
       and b2_opt = local_DLtoMidi t2 in
       let Tag(dur1, start1) as tag1 = getTag t1
       and Tag(dur2, start2) as tag2 = getTag t2
       in
       match (b1_opt, b2_opt) with
       | (None, None) -> None
       | (None, Some b2) -> Some b2
       | (Some b1, None) -> Some b1
       | (Some b1, Some b2) -> (
	 (** Computes the duration from the first event in the whole DList
	     to the first event of the DList in the product which does not
             hold the global first event, also return its sign. *) 
	 let (midi_offset, offset_sign) = match (start1, start2) with
	   | (Some st1, Some st2) -> 
	      let rel_offset = dur1 /+/ st2 /-/ st1 in
	      let midi_duration ~duration =
		MidiV.timeToSamplesNumber ~samplerate ~division
					  ~tempo ~duration in
	      (midi_duration (Time.abs rel_offset), Time.sign rel_offset)
	   | _ -> failwith "Empty but non-None MIDI.Multitrack.buffer"
	 in
	 let localDuration buf =
	   MIDI.Multitrack.duration buf in
	 let b1_dur = localDuration b1
	 and b2_dur = localDuration b2
	 in
	 let new_duration =
	   if offset_sign >= 0 then
	     max b1_dur (b2_dur + midi_offset)
	   else max (b1_dur + midi_offset) b2_dur
	 in
	 let new_multitrack = MIDI.Multitrack.create channels (new_duration) in
	 
	 let multitrack_adder offset multitrack1 multitrack2 =
	   for i = 0 to (channels-1) do
	     let partial_adder = MIDI.add new_multitrack.(i) in 
	     partial_adder (midi_offset*offset) multitrack1.(i) 0 new_duration;
	     partial_adder 0 multitrack2.(i) 0 new_duration
	   done
	 in
	 
	 (match offset_sign with
	  | -1 -> (** t2 starts first, shift t1. *)
	     multitrack_adder 1 b1 b2
	  | 0 -> (** Both tiles start at the same time. *)
	     multitrack_adder 0 b1 b2
	  | _ -> (** t1 starts first, shift t2. *)
	     multitrack_adder 1 b2 b1
	 );
	 Some(new_multitrack)
       )
     end
  | Modify (modifier, t) ->
     let new_context = let copy = Modify.Context.copy context in
		       Modify.replaceContext modifier copy; copy in
     toMidi ~channels ~samplerate ~division ~tempo ~context:new_context t
