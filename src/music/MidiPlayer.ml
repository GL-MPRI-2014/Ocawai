open OcsfmlAudio

let to_s16le c1 c2 =
  let b1 = int_of_char c1 in
  let b2 = int_of_char c2 in
  (b2 * 256) + b1

let play_midi_file fname run =
  let f = new MIDI.IO.Reader.of_file fname in
  let channels = 1 in
  let sample_rate = 44100 in
  let blen = 16384 in
  let buf = Audio.create channels blen in
  let samplesBuffer = Bigarray.Array1.create (Bigarray.int16_signed) Bigarray.c_layout blen in
  let mchannels = 16 in
  let mbuf = MIDI.Multitrack.create mchannels blen in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = new Synth.Multitrack.create mchannels (fun _ -> new Synth.square ~adsr sample_rate) in
  let eosound = new sound () in
  let oosound = new sound () in
  let r = ref (-1) in
  let i = ref 0 in
  let timer = ref 0. in
  let coeff = 0.865 in (* this is (exp(1)/pi) *)
  while !r <> 0 && !run do
    r := f#read sample_rate mbuf 0 blen;
    synth#play mbuf 0 buf 0 blen;
    let strs16le = Audio.S16LE.make buf 0 blen in
    for i = 0 to blen - 1 do
      samplesBuffer.{i} <- (to_s16le strs16le.[2*i] strs16le.[2*i + 1])
    done;

    if (!i mod 2 = 0) then begin
      let oobuf = new sound_buffer (`Samples(samplesBuffer,channels,sample_rate)) in
      (*oobuf#load_from_samples samplesBuffer channels sample_rate;*)
      oosound#set_buffer oobuf;
      Thread.delay (coeff *. !timer);
      (*obuf#load_from_samples samplesBuffer channels sample_rate;*)
      oosound#play;
      timer := OcsfmlSystem.Time.as_seconds (oobuf#get_duration)
    end
    else begin
      let eobuf = new sound_buffer (`Samples(samplesBuffer,channels,sample_rate)) in
      (*eobuf#load_from_samples samplesBuffer channels sample_rate;*)
      eosound#set_buffer eobuf;
      Thread.delay (coeff *. !timer);
      (*obuf#load_from_samples samplesBuffer channels sample_rate;*)
      eosound#play;
      timer := OcsfmlSystem.Time.as_seconds (eobuf#get_duration)
    end;

    incr i
  done;
  f#close
