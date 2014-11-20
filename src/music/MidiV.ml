(**
   Module MidiV
*)

(**
   Standard 44,1kHz samplerate
*)
let samplerate = 44100 

let division = MIDI.Ticks_per_quarter 96

(**
   Convert a 7 bit integer between 0 and 127 to a float
*) 
let velocityFromInt : int -> float =
  fun x ->
    let trim x = max 0. (min x 1.) in
    trim ((float_of_int x) /. 127.)
