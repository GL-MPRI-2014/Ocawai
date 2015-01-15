(**
   DList module used in music generation.
   DLists implement constant time insertion at the list's head as well as at its tail.

   Represents the background, lower-level implementation for the TPTM module.

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
   Encapsulate an event into a {b zero-duration} singleton,
   {i i.e.} a single element list
*)
val return : Music.event -> t

(** 
   Box the input [event] into a single event list,
   with [Start = 0 && Dur = duration event]
 *)
val returnWithDelay : Music.event -> t

(**
   @return the duration of the tile
*)
val duration : t -> Time.t

(**
   Modifies the input [t] and adds the input modifier for the rendering
 *)
val modify : Modify.t -> t -> t

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


(**
   Return a DList of the form :
   A sync from (Pos == 0) to start,
   |--> ...
   |--> The n-th event and the strictly positive sync to the (n+1)-th event,
   |--> ...
   |--> The last event and the (possibly negative) sync to Pos
   
   Obtained by iterating [headTail] on the input [t].
 *) 
val normalize : t -> t

(**
   Equality test

   Two DLists are equal iff there normal forms are equal
 *)
val is_equal : t -> t -> bool

(** {2 Advanced DList creation functions} *)

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
   Recursively converts a DList to a MIDI.Multitrack.buffer.

   @param int is the samplerate of the conversion
   @param MIDI.division is the chosen grid division
   @return the conversion of the given ['a t] into a MIDI event
*)
val toMidi : ?channels:int -> ?samplerate:int ->
	     ?division:MIDI.division -> ?tempo:Time.Tempo.t ->
	     ?context:Modify.Context.t -> t -> MIDI.Multitrack.buffer option
