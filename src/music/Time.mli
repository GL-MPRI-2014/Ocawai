(**
   Interface Time: defines a time unit [Time.t].

   [Time.t] is an ordered groupe
*)

type t

(** {2 Group operators} *)

(** Neutral value *)
val zero : t

(** Binary group operator *)
val plus : t -> t -> t

(** Group substraction *)
val minus : t -> t -> t

(** Group unary inverse *) 
val inverse : t -> t

(** {2 Comparison functions} *)

(** [compare t1 t2] @return a negative int
    if t1 is strictly less than t2,
    zero if t1 equals t2,
    a positive int if t1 is greater than t2
*)
val compare : t -> t -> int

(** Return the smaller of the two arguments. *)
val min : t -> t -> t

(** Return the greater of the two arguments. *)
val max : t -> t -> t
