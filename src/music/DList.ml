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

let getTag : t -> tag = function
  | Sync time -> Tag (time, None)
  | Event event -> Tag (Time.zero, Some Time.zero)
  | Prod (tag, _, _) -> tag

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

module MusicT =
  struct
    type t = event
    let compare = Music.compare
  end
    
module MusicSet = Set.Make(MusicT)

type headTail = {mutable to_events : time;
		  (* The distance from the entrance of the head to
		     the events it holds. *)
		 mutable events : MusicSet.t;
		 mutable to_next : time;
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

and fprintf_sub : Format.formatter -> t -> unit = fun fmt -> function
  | Event event -> Format.fprintf fmt "@[<1>Event(%a@,)@]" Music.fprintf event
  | Sync dur -> Format.fprintf fmt "@[<1>Sync(%a@,)@]" Time.fprintf dur
  | Prod(tag, t1, t2) ->
    Format.fprintf fmt "@[Prod(@[%a,@ %a,@ %a@]@,)@]" fprint_tag tag fprintf_sub t1 fprintf_sub t2

and fprint_tag fmt = function
  | Tag(dur, start) -> Format.fprintf fmt "@[<1>Tag(@[Dur =@ %a,@ Start =@ %a@]@,)@]"
    Time.fprintf dur fprint_start start

and fprint_start fmt = function
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
  let printer = Format.pp_print_list Music.fprintf in
  Format.fprintf fmt "@[<1>MusicSet(%a@,)@]" printer (MusicSet.elements set) 


let printf : t -> unit = fprintf Format.std_formatter


(** {2 Normalization functions} *)

let rec headTail_tuple : t -> headTail =
  (** Returns a tuple containing a decomposition of
      the input tail where :
      the "head" holds the first 'real' events of
      the tile (ie. no syncing), the tail
      contains the remaining events.
      to_events and to_next are the syncing plumbing
      needed to rebuild the tile correctly.
      This in an auxiliary function with the plumbing exposed.
  *)
  (* New stuff *)
  let aggregate ht1 ht2 =
    let to_next1 = ht1.to_next
    and to_events2 = ht2.to_events in
    ht1.to_next <- to_next1 /+/ to_events2;
    ht2.to_events <- Time.zero
    (* End of new stuff *)
  in
  function
  | Sync dur ->
    makeHeadTail Time.zero (MusicSet.empty) dur zero
  | Event event ->
    makeHeadTail Time.zero (MusicSet.singleton event) Time.zero zero
  | Prod ((Tag (dur, start)), t1, t2) ->
    let Tag(dur1, startT1) = getTag t1
    and Tag(dur2, startT2) = getTag t2 in
    match (startT1, startT2) with
    | (None, None) ->
      (** Both factors are pure sync, return the total sync. *)
      headTail_tuple (sync dur)
    | (None, Some _) ->
      (** t1 is pure sync, shift t2. *)
      let headTuple2 = headTail_tuple t2
      in (headTuple2.to_events <- (dur1 /+/ headTuple2.to_events);
	  headTuple2)
    | (Some _, None) ->
      (** t2 is pure sync. *)
      let headTuple1 = headTail_tuple t1
      in if (Time.sign (headTuple1.to_next) <= 0)
        then (** There are no more events in t1's tail and t2 is empty :
        	 the tail is pure sync. *)
          ( headTuple1.to_next <- dur2;
	    headTuple1 )
        else ( headTuple1.tailT <- (headTuple1.tailT) /::/ (sync dur2);
	       headTuple1 )

    | (Some start1, Some start2) ->
      let shifted_start2 = start2 /+/ dur1 in
      match (let comp = Time.compare start1 shifted_start2 in
	     (comp < 0, comp > 0)) with
      | (true, false) -> (* t1 starts first *)
        let headTuple1 = headTail_tuple t1 in
	let next_of_head = start1 /+/ headTuple1.to_next in
	if (* next_of_head <= shifted_start2 *)
	  ((Time.compare next_of_head shifted_start2) <= 0)
	then
	    ( headTuple1.tailT <- headTuple1.tailT /::/ t2;
	      headTuple1 )
          else (
	    headTuple1.to_next <- shifted_start2 /-/ start1;
	    (* Note that we have to_next > 0 here. *)
            headTuple1.tailT <- (sync (next_of_head /-/ shifted_start2))
            /::/ headTuple1.tailT /::/ t2;
	    headTuple1)

      | (false, false) -> (* The first events in both t1 and t2 are synchronized *)
	let headTuple1 = headTail_tuple t1
	and headTuple2 = headTail_tuple t2 in
        let sync_endTail1ToNext2 =
          (Time.inverse dur1) /+/ headTuple1.to_events /+/ headTuple2.to_next in
	let newHT =
          makeHeadTail headTuple1.to_events (MusicSet.union headTuple1.events headTuple2.events)
            headTuple1.to_next
            (headTuple1.tailT /::/ (sync sync_endTail1ToNext2)
             /::/ headTuple2.tailT)
        in
        if ((Time.compare headTuple2.to_next headTuple1.to_next) < 0) then
          ( newHT.to_next <- headTuple2.to_next;
	    newHT.tailT <- (sync (headTuple1.to_next /-/ headTuple2.to_next)) /::/
              headTuple1.tailT /::/
              (sync sync_endTail1ToNext2) /::/ headTuple2.tailT;
	    newHT)
        else newHT
	  
      | (false, true) -> (* t2 starts first *)
	let headTuple1 = headTail_tuple t1 in
	let headTuple2 = headTail_tuple t2 in
        let next_of_head = shifted_start2 /+/ headTuple2.to_next in
	let newHT = makeHeadTail shifted_start2 headTuple2.events in
        if (* next_of_head <= start1 *)
	  (Time.compare next_of_head start1 <= 0) then
            newHT headTuple2.to_next
	      (
		(sync (Time.inverse next_of_head)) /::/
		  t1 /::/
		  (sync (headTuple2.to_events /+/ headTuple2.to_next)) /::/
		  headTuple2.tailT
	      )
	  else newHT ((Time.inverse shifted_start2) /+/ headTuple1.to_events) (
            t1 /::/
              (sync (headTuple2.to_events /+/ headTuple2.to_next)) /::/
              headTuple2.tailT
	  )
      | _ -> failwith "Cannot happen."

let headTail : t -> t * t =
  (* Return the head and the tail of the input tile, with all the plumbing
     applied and hidden. *)
  fun t ->
    let headTailT = headTail_tuple t in
    Format.fprintf Format.std_formatter
		   "@[Extracted one head-tail-tuple :@ %a@]@." fprint_headTail headTailT;
    let head = (sync (headTailT.to_events)) /::/
      fromList_parallel (MusicSet.elements headTailT.events) /::/ (sync headTailT.to_next)
    in (head, headTailT.tailT)

let rec normalize : t -> t = fun t -> 
  let (head, tail) = headTail t in
  if isZero tail then
    head
  else head /::/ normalize tail

(** Equality function *)

let rec is_silent : t -> bool = function
  | Sync _ -> true
  | Event (event) -> Music.is_silent event
  | Prod (tag, t1, t2) -> is_silent t1 && is_silent t2

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
  
(*
(**
   Equality modulo observational equivalency
 *)
let rec is_equivalent : t -> t -> bool = fun t1 t2 ->
  match (t1, t2) with
  | (Rest dur1, Rest dur2) -> Time.is_equal dur1 dur2
  | (Rest dur1, Event _ as event) -> (Time.equal dur1 Time.zero) &&
					       is_silent event
  | (Rest dur1, Prod (dur2, 
 *)				

(*
type normalized_t = NormSync of time
		  | NormEvent of event
		  | NormProd of (tag * t * t)

let normalize : t -> normalized_t
 *)
  
(** {2 MIDI conversion} *)

(**
   Converts a DList to a MIDI.buffer

   Semantics : the MIDI.buffer's beginning is the first event in the DList
 *)
let rec toMidi : ?samplerate:int -> ?division:MIDI.division ->
		 ?tempo:Time.Tempo.t -> t -> MIDI.buffer option =
  fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
      ?tempo:(tempo = Time.Tempo.base) ->
  let local_musicToMidi : Music.event -> MIDI.buffer =
    Music.toMidi ~samplerate ~division ~tempo
  in 
  function
  | Event event -> Some(local_musicToMidi event)
  | Sync dur -> None
  | Prod (Tag(dur, _), t1, t2) (* as t *) ->
     (*
     print_string "Converting DList product to midi\n";
     printf t;
      *)     
     let local_DLtoMidi : t -> MIDI.buffer option =
       toMidi ~samplerate ~division ~tempo in
     let b1_opt = local_DLtoMidi t1 
     and b2_opt = local_DLtoMidi t2 in
     let Tag(dur1, start1) as tag1 = getTag t1
     and Tag(dur2, start2) as tag2 = getTag t2
     in
     match (b1_opt, b2_opt) with
     | (None, None) -> (* print_string "Conversion completed";
			*) None
     | (None, Some b2) -> (* print_string "Conversion completed"; *) Some b2
     | (Some b1, None) -> (* print_string "Conversion completed"; *) Some b1
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
	 | _ -> failwith "Empty but non-None MIDI.buffer"
       in
       let localDuration buf =
	 MIDI.Multitrack.duration [|buf|] in 
       let b1_dur = localDuration b1
       and b2_dur = localDuration b2
       in
       let new_duration =
	 if offset_sign >= 0 then
	   max b1_dur (b2_dur + midi_offset)
	 else max (b1_dur + midi_offset) b2_dur
       in
       let new_buffer = MIDI.create(new_duration) in
       
       (match offset_sign with
	| -1 -> (** t2 starts first, shift t1. *)
	   MIDI.add new_buffer midi_offset b1 0 new_duration;
	   MIDI.add new_buffer 0 b2 0 new_duration
	| 0 -> (** Both tiles start at the same time. *)
	   MIDI.add new_buffer 0 b1 0 new_duration;
	   MIDI.add new_buffer 0 b2 0 new_duration
	| _ -> (** t1 starts first, shift t2. *)
	   MIDI.add new_buffer midi_offset b2 0 new_duration;
	   MIDI.add new_buffer 0 b1 0 new_duration
       );
       (* print_string "Conversion completed"; *) Some(new_buffer)
     )
