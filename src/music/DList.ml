(**
   DList module
*)

open Music

type time = Num.num

type 'a event = 'a Music.t

type tag = Tag of (time * (time option))

type 'a t = Sync of time
	 | Event of 'a event
	 | Prod of (tag, 'a t, 'a t)

let sync : time -> 'a t = fun time -> Sync time

let zero : 'a t = Sync (Num.Int 0)

let isZero : 'a t -> bool = fun t -> zero

let return : 'a event -> 'a t = fun event -> Event event

(*
let tagProd : tag -> tag -> tag =
  fun (Tag (d1, start1)) (Tag (d2, start2)) ->
    let newStart = match (start1, start2) with
      | (None, None) -> None
      | (

let (/::/) : 'a t -> 'a t -> 'a t = fun t1 t2 ->
*)
