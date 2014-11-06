(**
   Module Time, implements the time unit used for the music
*)

open Num

(** The time-unit is the rationals *)
type t = num

let zero = Int(0)

let plus = (+/)

let minus = (-/)

let inverse = minus_num

let compare = compare_num

let min = min_num

let max = max_num
