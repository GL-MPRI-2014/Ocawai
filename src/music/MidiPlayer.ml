open OcsfmlAudio

let to_s16le c1 c2 =
  let b1 = int_of_char c1 in
  let b2 = int_of_char c2 in
  (b2 * 256) + b1

let play_midi_file fname run =
  let f = new MIDI.IO.Reader.of_file fname in
  let channels = 1 in
  let sample_rate = 44100 in
  let blen = 4094 in
  let buf = Audio.create channels blen in
  let samplesBuffer = Bigarray.Array1.create (Bigarray.int16_signed) Bigarray.c_layout blen in
  let mchannels = 16 in
  let mbuf = MIDI.Multitrack.create mchannels blen in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = new Synth.Multitrack.create mchannels (fun _ -> new Synth.square ~adsr sample_rate) in
  let agc = Audio.Effect.auto_gain_control channels sample_rate ~volume_init:0.3 () in
  let osound = new sound () in
  let r = ref (-1) in
  let coeff = 0.9 in (* Magic number ... Can't seem to do better than this ! *)
  while !r <> 0 && !run do
    r := f#read sample_rate mbuf 0 blen;
    synth#play mbuf 0 buf 0 blen;
    agc#process buf 0 blen;
    let strs16le = Audio.S16LE.make buf 0 blen in
    for i = 0 to blen - 1 do
      samplesBuffer.{i} <- (to_s16le strs16le.[2*i] strs16le.[2*i + 1])
    done;
    let obuf = new sound_buffer (`Samples(samplesBuffer,channels,sample_rate)) in
    let timer = OcsfmlSystem.Time.as_seconds (obuf#get_duration) in
    osound#set_buffer obuf;
    osound#play;
    Thread.delay (coeff *. timer);
  done;
  f#close
