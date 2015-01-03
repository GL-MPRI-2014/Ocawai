(**
   Module MidiV holding shared functions and constants for the manipulation of
   MIDI buffers within the framework defined by the mm-library
*)

(**
   Default samplerate to use for the MIDI conversion
 *)
val samplerate : int

(**
   Default MIDI-division value to use for the MIDI conversion
 *)
val division : MIDI.division

(**
   Convert a 7 bit integer between 0 and 127 to a float
*)
val velocityFromInt : int -> float

(**
   Converts a duration in [Time.t] into an integer number of MIDI pulses,
   with respect to the given [tempo]

  @param samplerate A value in samples per second 
*)
val timeToSamplesNumber : ?samplerate:int -> ?division:MIDI.division ->
			  ?tempo:Time.Tempo.t -> duration:Time.t -> int
