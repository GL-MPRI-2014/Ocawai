(**
   Instrument module

   Exports several instrument types, and their synthesized version
 *)

(**
   The type of instruments
 *)
type t = Kick | Snare | Organ

(**
   The default instrument to which to assign unassigned channels 
 *)
val default : t

(**
   Assigns each instrument to a MIDI channel

   TO-DO : check compliancy with standard MIDI
 *)
val to_channel : t -> int

(**
   Returns a synthesizer associated to the inupt [Instrument.t]
 *)
val to_synth : ?samplerate:int -> t -> Synth.t

(**
   Instrument ordering function

   Simply replicates the order in which the constructors are defined in the
   definition of [t]
   Same specification as [Pervasives.compare]
 *)
val compare : t -> t -> int

(**
   Pretty prints the input [event] and outputs to the channel
   defined by the [Format.formatter]
*)
val fprintf : Format.formatter -> t -> unit
