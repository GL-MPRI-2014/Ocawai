(**
   Interface Time: defines a time unit [Time.t].

   [Time.t] is an ordered groupe used for the description of durations
   in the music tiles. 

   Also holds the [Time.Tempo] module, which defines tempi and conversion
   functions for MIDI.
*)

type t

exception Unsupported_MIDI_division

(** {2 Group operators} *)

(** Neutral value *)
val zero : t

(** Binary group operator *)
val plus : t -> t -> t

(** Group substraction *)
val minus : t -> t -> t

(** Group unary inverse *) 
val inverse : t -> t

(** Return [-1], [0] or [1] according to the sign of the argument. *)
val sign : t -> int

(** {2 Comparison functions} *)

(** Same specification as [Pervasives.compare]

    @return [-1] if [t1] is strictly less than [t2],
    [0] if [t1] equals [t2],
    [+1] if [t1] is strictly greater than [t2]
*)
val compare : t -> t -> int

(** Equality test *)
val is_equal : t -> t -> bool

(** Return the smaller of the two arguments. *)
val min : t -> t -> t

(** Return the greater of the two arguments. *)
val max : t -> t -> t

(** Absolute value *)
val abs : t -> t

(** {2 Basic time creation} *)

(**
   @return the time associated to the input integer
 *)
val fromInt : int -> t

(**
   @return the time associated to the quotient of the input integers
 *)
val fromPair : int * int -> t

val bn : t (** brevis *)
val wn : t (** whole note = une ronde*)
val hn : t (** half note = une blanche *)
val qn : t (** quarter note = une noire *)
val en : t (** eighth note = une croche *)
val sn : t (** sixteenth note *)
val tn : t (** thirty-second note *)
val sfn : t (** sixty-fourth note *)
val tren : t (** triplets = tiers de triolet de croches *)

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

val toInt : t -> int

val toNum : t -> Num.num

(**
   @param division the MIDI division, given in ticks per quarter
   
   @return the number of MIDI ticks corresponding to the duration d
   Raises [!Unsupported_MIDI_division] if called with a [MIDI.division]
   type other than [MIDI.Ticks_per_quarter].
 *)
val toMidiTicks : division:MIDI.division -> t -> int

(** {2 Tempo definition and management} *)

module Tempo : sig
  (**
   Tempo definition module.

   A tempo is seen as a ratio between the wanted value of tempo and
   a base tempo, defined as 120 BPM.
   This model allows compositional management of tempi and straightforward
   accelerations / decelerations.
   *)

  type t

  (** Input integer tempo value
      @return the [int] Beats Per Minute tempo *)
  val fromInt : int -> t

  (** {2 Basic values} *)

  (** The default tempo value, 120 BPM *)
  val base : t

  (** {2 Tempo conversions} *)

  (**
   @return the conversion to microseconds per quarter of the input [tempo] ratio
   *)
  val toMicrosecondsPerQuarters : t -> int
end

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

val fprintf : Format.formatter -> t -> unit
