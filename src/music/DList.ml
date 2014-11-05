(**
   DList module
*)

open Music
open Num

type event = Music.t Music.note
type time = Num.num

let zero_time : time = Num.Int 0

(** Tag manipulation functions *)

type tag = Tag of (time (** Value dur *)
		   * (time option) (** Value start*)
)

let getTag : t -> tag = function
  | Sync time -> Tag (time, None)
  | Event event -> Tag (zero_time, Some zero_time)
  | Prod (tag, _, _) -> tag

let (tagProd) : tag -> tag -> tag =
  fun (Tag (dur1, start1)) (Tag (dur2, start2)) -> 
    let newStart = match (start1, start2) with
      | (None, None) -> None
      | (None, Some start) -> Some (st_time +/ dur2)
      | (Some st_time, None) -> Some st_time
      | (Some st_time1, Some st_time2) -> Some (min_num st_time1 (st_time2 +/ dur2))
    in Tag (dur1, start1)

(** Base DLists *)

type t = Sync of time
	 | Event of event
	 | Prod of (tag * t * t)

let sync : time -> t = fun time -> Sync time

let zero : t = Sync zero_time

let isZero : t -> bool = fun t -> t = zero

let return : event -> t = fun event -> Event event

let returnWithDelay : event -> t = fun event ->
  let dur = Note.getDur event in
  (return event) /::/ (sync dur)

(** DList manipulation functions *)

let (/::/) : t -> t -> t = fun t1 t2 ->
  if isZero t1 then t2
  else if izZero t2 then t1
  else
    let Tag(dur, start) = (getTag t1) tagProd (getTag t2) in
    if start = None then
      Sync dur
    else Prod newTag t1 t2

(** Normalization functions *)

type HeadTail = {toEvents : time,
		 (* The distance from the entrance of the head to
		    the events it holds. *)
		 events : event Set.t,
		 toNext : time,
		 tailT : t
		}

let HeadTail time 

headTail_tuple : t -> headTail =
  (** Returns a tuple containing a decomposition of
      the input tail where :
      the "head" holds the first 'real' events of
      the tile (ie. no syncing), the tail
      contains the remaining events.
      toEvents and toNext are the syncing plumbing
      needed to rebuild the tile correctly.
      This in an auxiliary function with the plumbing exposed.
  *)
  function
  | Event a =
  HeadTail mzero (Set.singleton a) mzero unitList
  | Sync d  =
  HeadTail mzero Set.empty d unitList
  | Prod ((Tag (dur, start)), t1, t2) =
  let Tag(dur1, startT1) = getTag t1
  and Tag(dur2, startT2) = getTag t2
  in
  match (startT1, startT2) of
      (None, None) ->
    -- Both factors are pure sync, return the total sync.
      headTail_tuple $ Sync dur
      
        (None, Some _) ->
          -- t1 is pure sync, shift t2.
            let headTuple2 = headTail_tuple t2
            in headTuple2 {toEvents = dur1 `mplus` toEvents headTuple2}

        (Some _, None) ->
          -- t2 is pure sync.
            let headTuple1 = headTail_tuple t1
            in if toNext headTuple1 <= mzero
               then -- There are no more events in t1's tail and t2 is empty :
                    -- the tail is pure sync.
                   headTuple1 {toNext = dur2}
               else headTuple1 {tailT = (tailT headTuple1) +++ (sync dur2)}
                    
        (Some start1, Some start2) ->
            let shifted_start2 = start2 `mplus` dur1 in
            case (start1 `compare` shifted_start2) of
              LT ->  {- t1 starts first -}
                  let headTuple1@
                        (HeadTail toEventsH1 eventsH1 toNextH1 tailTH1) =
                          headTail_tuple t1
                      nextOfHead = start1 `mplus` toNextH1
                  in if nextOfHead <= shifted_start2 then
                         headTuple1 { tailT = tailTH1 +++ t2 }
                     else headTuple1
                              {toNext = shifted_start2 `mminus` start1,
                               -- Note that we have toNext > 0 here.
                               tailT =
                                   (sync $ nextOfHead `mminus` shifted_start2)
                                   +++ tailTH1 +++ t2}

              EQ -> {- The first events in both t1 and t2 are synchronized -}
                  let headTuple1@
                        (HeadTail toEventsH1 eventsH1 toNextH1 tailTH1 ) =
                          headTail_tuple t1
                      headTuple2@
                        (HeadTail _ eventsH2 toNextH2 tailTH2 ) =
                          headTail_tuple t2
                      sync_endTail1ToNext2 =
                          (minverse dur1) `mplus` toEventsH1 `mplus` toNextH2
                      newHT =
                          HeadTail toEventsH1 (eventsH1 `Set.union` eventsH2)
                          toNextH1
                          (tailTH1 +++ (sync $ sync_endTail1ToNext2)
                           +++ tailTH2)
                  in
                    if toNextH2 < toNextH1 then
                        newHT {toNext = toNextH2,
                               tailT = (sync $ toNextH1 `mminus` toNextH2) +++
                                       tailTH1 +++
                                       (sync sync_endTail1ToNext2) +++ tailTH2}
                    else newHT

              GT -> {- t2 starts first -}
                  let headTuple1@
                        (HeadTail toEventsH1 eventsH1 toNextH1 tailTH1) =
                          headTail_tuple t1
                      headTuple2@
                        (HeadTail toEventsH2 eventsH2 toNextH2 tailTH2) =
                          headTail_tuple t2
                      nextOfHead = shifted_start2 `mplus` toNextH2
                      newHT = HeadTail shifted_start2 eventsH2
                  in if nextOfHead <= start1 then
                         newHT toNextH2 $
                                   (sync $ minverse nextOfHead) +++
                                   t1 +++
                                   (sync $ toEventsH2 `mplus` toNextH2) +++
                                   tailTH2
                     else newHT (minverse shifted_start2 `mplus` toEventsH1) $
                            t1 +++
                            (sync $ toEventsH2 `mplus` toNextH2) +++
                            tailTH2


headTail :: (OrdGrp time, Ord a) =>
            DList time a -> (DList time a, DList time a)

-- Return the head and the tail of the input tile, with all the plumbing
-- applied and hidden. 

headTail t =
    let (headTuple@(HeadTail toEvents eventsH toNext tail)) = headTail_tuple t
        head = (sync $ toEvents) +++
               fromList (Set.toList eventsH) +++ (sync toNext)
    in (head, tail)

(** Testing functions *)

let fromList_chord : event list -> t =
  List.fold_left
    (fun acc event -> acc /::/ return event) zero

let fromList_sequence : event list -> t =
  List.fold_left
    (fun acc event -> acc /::/ returnWithDelay event) zero
