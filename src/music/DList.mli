(**
   DList module used in music generation.
   DLists implement constant time insert in head AND tail.

   Represents the background, lower-level implementation for the Tile module.

   Based on the TPTM model developed by P. Hudak and
   D. Janin, code inspired by Theis Bazin's work on this model.
   @author "Theis Bazin" @author "Mathias Sable Meyer"
*)

type t

(** {2 Base DLists} *)

(**
   Delay to sync various tiles
*)
val sync : Time.t -> t

(**
   Neutral element for the Tiled product
*)
val zero : t

(**
   Compares t to zero
*)
val isZero : t -> bool

(**
   Encapsulate an event into a singleton, that is a single element list
*)
val return : Music.event -> t

(**
   @return the duration of the tile
*)
val duration : t -> Time.t

(** {2 DList operators} *)

(**
   Infix operation for concatenation
*)
(*  
    Eventually this infix operator should be modified, we wanted ::: but somehow
    this is not possible ...
*)
val (/::/) : t -> t -> t

(** {2 Normalization functions} *)

(**
   @return [(head, tail)] of the input [tile] with respect to the time
*)
val headTail : t -> t * t

(** {2 Testing functions} *) 

(**
   @return the tile containing all events in the [Music.event list] as a chord
*)
val fromList_parallel : Music.event list -> t

(**
   @return the tile containing all events in the [Music.event list] in sequence
*)
val fromList_sequence : Music.event list -> t

(** {3 Pretty_printing} *)

(**
   Pretty prints the input [t] and outputs to the channel
   defined by the [Format.formatter]
*)
val fprintf : Format.formatter -> t -> unit

(**
   Recursively converts DList to a MIDI.buffer.

   @param int is the samplerate of the conversion
   @param MIDI.division is the chosen grid division
   @return the conversion of the given ['a t] into a MIDI event
*)
val toMidi : ?samplerate:int -> ?division:MIDI.division ->
	     tempo:Time.Tempo.t -> t -> MIDI.buffer option
