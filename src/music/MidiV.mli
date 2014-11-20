(**
   Module MidiV holding shared functions and constants for the manipulation of
   MIDI buffers within the framework defined by the mm-library
*)

val samplerate : int

val division : MIDI.division

(**
   Convert a 7 bit integer between 0 and 127 to a float
*)
val velocityFromInt : int -> float

(**
   Converts a duration in [Time.t] into an integer number of MIDI pulses,
   with respect to the given [tempo] 
*)
val duration : ?samplerate:int -> ?division:MIDI.division ->
  Time.t -> ~tempo:float -> int
