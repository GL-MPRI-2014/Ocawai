(**
   Defines the basic elements for music generation.

   Defines types ['a Music.t] and [Music.param] :
   the first one is a single note with parameters in ['a],
   the second is a type for parameters, one then builds
   music with the base type [Music.param Music.t]

   Borrows much from @author "Paul Hudak"'s Haskell library Euterpea 
*)

(** {2 Type definitions} *)

type 'a t = Note of (Time.t * 'a)
	    | Rest of Time.t

type octave = int
type pitchClass  = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
		   | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
		   | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
		   | Bf | Ass | B | Bs | Bss
type pitch = pitchClass * octave
type velocity = int

val pitch_to_string : pitch -> string 
  
(**
   The class [param] defines an instantiation for the type ['a t]

   We use a class here for extensivity.
*)
class param : pitch -> velocity ->
	      object
		val mutable pitch : pitch (** The note's pitch *)
		val mutable velocity : velocity (** The note's velocity *)
		
		method pitch : pitch (** Get note's pitch *)
		method velocity : velocity (** Get note's velocity *)

		method setPitch : pitch -> unit  (** Set note's pitch *)
		method setVelocity : velocity -> unit  (** Set note's velocity *)
	      end

(**
   The instantiation of ['a t] used in other modules
*)
type event = param t

(** {2 Basic Music creation} *)

(**
   @return the note of duration [time] with parameter ['a]
*)
val note : Time.t -> 'a -> 'a t 

(**
   @return a rest of duration [Time.t]
*)
val rest : Time.t -> 'a t

(**
   @return the length of an event
*)
val getDur : 'a t -> Time.t

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

(**
   Pretty prints the input [event] and outputs to the channel
   defined by the [Format.formatter]
*)
val fprintf : Format.formatter -> event -> unit

(** {2 MIDI conversion}
    Based on @author "Savonet"'s mm Midi event type
*)

(**
   Used for the rendering of music

   @param int is the samplerate of the conversion
   @param MIDI.division is the chosen grid division
   @return the conversion of the given ['a t] into a MIDI event
*)
val toMidi : ?samplerate:int -> ?division:MIDI.division ->
	     tempo:Time.Tempo.t -> event -> MIDI.buffer
