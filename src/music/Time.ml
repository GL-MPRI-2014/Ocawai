(**
   Module Time, implements the time unit used for the music
*)

open Num

(** The time-unit is the rationals *)
type t = num

let zero = Int(0)

let plus = ( +/ )

let minus = ( -/ )

let inverse = minus_num

let compare = compare_num

let min = min_num

let max = max_num

(** {2 Basic time creation} *)

let fromPair : (int * int) -> t = function
  | (a, b) -> Num.( // ) (Int a) (Int b)

let bn : t = fromPair (2, 1)
let wn : t = fromPair (1, 1)
let hn : t = fromPair (1, 2)
let qn : t = fromPair (1, 4)
let en : t = fromPair (1, 8)
let sn : t = fromPair (1, 16)
let tn : t = fromPair (1, 32)
let sfn : t = fromPair (1, 64)
let dwn : t = fromPair (3, 2)
let dhn : t = fromPair (3, 4)
let dqn : t = fromPair (3, 8)
let den : t = fromPair (3, 16)
let dsn : t = fromPair (3, 32)
let dtn : t = fromPair (3, 64)
let ddhn : t = fromPair (7, 8)
let ddqn : t = fromPair (7, 16)
let dden : t = fromPair (7, 32)

(** {2 Time conversions} *)

(**
   @return a floating-point approximation of the input [t]
 *)
let toFloat : t -> float = Num.float_of_num
let toInt : t -> int = Num.int_of_num

(** {2 Tempo definition and management} *)

module Tempo = struct
  (**
   Tempo module
   *)

  open Num

  type t = Num.num

  (** {2 Basic values} *)

  (** The basic tempo ratio 1, defines a tempo of 120BPM *)
  let base : t = Num.Int 1

  (** {2 Tempo conversions} *)

  let tempoToMspq : t -> int = function
    | tempo -> Num.int_of_num (
		   let baseTempo = Num.Int 120 in
		   baseTempo */ tempo
		 )

end

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

let fprintf : Format.formatter -> t -> unit = fun fmt ->
  function
  | num ->
     Format.fprintf fmt "@[%s@]" (Num.string_of_num num)
