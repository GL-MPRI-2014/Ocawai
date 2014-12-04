(**
   DList module
*)

open Music
open Format

type time = Time.t
type event = Music.event

let (/+/) : time -> time -> time = fun x y -> (Time.plus x y)
let (/-/) : time -> time -> time = fun x y ->  (Time.minus x y)

(** Tag manipulation functions *)

type tag = Tag of (time (** Value dur *)
		   * (time option) (** Value start*)
)

(** Base DLists *)

type t = Sync of time
	 | Event of Music.event
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
      | (Some st_time1, Some st_time2) -> Some (Time.min st_time1 (st_time2 /+/ dur2))
    in Tag (dur1 /+/ dur2, newStart)

let sync : time -> t = fun time -> Sync time

let zero : t = sync Time.zero

let isZero : t -> bool = fun t -> t = zero

let return : Music.event -> t = fun event -> Event event

let getDur : t -> Time.t = fun t ->
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
  let dur = Music.getDur event in
  (return event) /::/ (sync dur)

(** List <-> DList functions *)

let fromList_parallel : Music.event list -> t =
  List.fold_left
    (fun acc event -> acc /::/ return event) zero

let fromList_sequence : Music.event list -> t =
  List.fold_left
    (fun acc event -> acc /::/ returnWithDelay event) zero

(** Normalization functions *)

module MusicT =
  struct
    type t = event
    let compare = Pervasives.compare
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
      in if (headTuple1.to_next <= Time.zero)
        then (** There are no more events in t1's tail and t2 is empty :
		 the tail is pure sync. *)
          ( headTuple1.to_next <- dur2;
	    headTuple1 )
        else ( headTuple1.tailT <- (headTuple1.tailT) /::/ (sync dur2);
	       headTuple1 )

    | (Some start1, Some start2) ->
      let shifted_start2 = start2 /+/ dur1 in
      match (let comp = compare start1 shifted_start2 in
	     (comp < 0, comp > 0)) with
      | (true, false) -> (* t1 starts first *)
        let headTuple1 = headTail_tuple t1 in
	let next_of_head = start1 /+/ headTuple1.to_next in
	if next_of_head <= shifted_start2 then
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
        if (headTuple2.to_next < headTuple1.to_next) then
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
        if next_of_head <= start1 then
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
    let head = (sync (headTailT.to_events)) /::/
      fromList_parallel (MusicSet.elements headTailT.events) /::/ (sync headTailT.to_next)
    in (head, headTailT.tailT)

(** {2 Testing functions} *)

(** {3 Pretty_printing} *)

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

let printf : t -> unit = fprintf Format.std_formatter
