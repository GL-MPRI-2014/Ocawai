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

(** {2 Basic time creation} *)

(**
   @return the time associated to the quotient of the input integers
*)
val fromPair : int * int -> t

val bn : t (** brevis *)
val wn : t (** whole note *)
val hn : t (** half note *)
val qn : t (** quarter note *)
val en : t (** eighth note *)
val sn : t (** sixteenth note *)
val tn : t (** thirty-second note *)
val sfn : t (** sixty-fourth note *)

val dwn : t (** dotted whole note *)
val dhn : t (** dotted half note *)
val dqn : t (** dotted quarter note *)
val den : t (** dotted eighth note *)
val dsn : t (** dotted sixteenth note *)
val dtn : t (** dotted thirty-second note *)

val ddhn : t (** double-dotted half note *)
val ddqn : t (** double-dotted quarter note *)
val dden : t (** double-dotted eighth note *)

(** {2 Time conversions} *)

(**
   @return a floating-point approximation of the input [t]
*)
val toFloat : t -> float 

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

val fprintf : Format.formatter -> t -> unit
