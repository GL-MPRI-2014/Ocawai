(**
   DList module
*)

open Music
open Num

type time = Num.num

let zero_time : time = Num.Int 0

type event = Music.t Music.note

type tag = Tag of (time (** Value dur *)
		   * (time option) (** Value start*)
)

type t = Sync of time
	 | Event of event
	 | Prod of (tag * t * t)

let sync : time -> t = fun time -> Sync time

let zero : t = Sync zero_time

let isZero : t -> bool = fun t -> t = zero

let return : event -> t = fun event -> Event event

let (tagProd) : tag -> tag -> tag =
  fun (Tag (dur1, start1)) (Tag (dur2, start2)) -> 
    let newStart = match (start1, start2) with
      | (None, None) -> None
      | (None, Some start) -> Some (st_time +/ dur2)
      | (Some st_time, None) -> Some st_time
      | (Some st_time1, Some st_time2) -> Some (min_num st_time1 (st_time2 +/ dur2))
    in Tag (dur1, start1)

let getTag : t -> tag = function
  | Sync time -> Tag (time, None)
  | Event event -> Tag (zero_time, Some zero_time)
  | Prod (tag, _, _) -> tag

let (/::/) : t -> t -> t = fun t1 t2 ->
  if isZero t1 then t2
  else if izZero t2 then t1
  else
    let Tag(dur, start) = (getTag t1) tagProd (getTag t2) in
    if start = None then
      Sync dur
    else Prod newTag t1 t2


