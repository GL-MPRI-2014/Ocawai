(**
   Module MidiV
*)

(**
   Standard 44,1kHz samplerate
*)
let samplerate = 44100

(**
   Standard MIDI-division value
 *)
let division = MIDI.Ticks_per_quarter 96

(**
   Convert a 7 bit integer between 0 and 127 to a float
*) 
let velocityFromInt : int -> float =
  fun x ->
    let trim x = max 0. (min x 1.) in
    trim ((float_of_int x) /. 127.)

(** Code courtesy of the savonet project *)

type delta = int

(* Tempo is in microseconds per quarter. *)
let samples_of_delta samplerate division tempo delta =
  match division with
  | MIDI.Ticks_per_quarter tpq ->
     (* These computations sometimes overflow on 32 bits. *)
     let tpq = Int64.of_int tpq in
     let tempo = Int64.of_int tempo in
     let tps = Int64.of_int samplerate in
     let ten = Int64.of_int 1000000 in
     let delta = Int64.of_int delta in
     let ( * ) = Int64.mul in
     let ( / ) = Int64.div in
     Int64.to_int ((((delta * tempo) / tpq) * tps) / ten)
  | MIDI.SMPTE (fps,res) ->
     (samplerate * delta) / (fps * res)

let timeToMidiDuration : ?samplerate:int -> ?division:MIDI.division ->
			 ?tempo:Time.Tempo.t -> duration:Time.t -> int =
  fun ?samplerate:(sr = samplerate) ?division:(div = division)
      ?tempo:(tempo = Time.Tempo.base) ~duration ->
  let tempo_mspq = Time.Tempo.tempoToMspq tempo
  and delta = Time.toInt duration in
  samples_of_delta sr div tempo_mspq delta