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
  let samplesBuffer = Bigarray.Array1.create (Bigarray.Int16_signed) Bigarray.c_layout blen in
  let mchannels = 16 in
  let mbuf = MIDI.Multitrack.create mchannels blen in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = new Synth.Multitrack.create mchannels (fun _ -> new Synth.saw ~adsr sample_rate) in
  let osound = new sound () in
  let obuf = new sound_buffer `None in
  osound#set_buffer obuf;
  let r = ref (-1) in
  while !r <> 0 && !run do
    let i = ref 0 in
    r := f#read sample_rate mbuf 0 blen;
    synth#play mbuf 0 buf 0 blen;
    let strs16le = Audio.S16LE.make buf 0 blen in
    for i = 0 to blen - 1 do
      let encoded = to_s16le strs16le.[2*i] strs16le.[2*i + 1] in
      samplesBuffer.{i} <- (encoded)
    done;
    let obuf = new sound_buffer (`Samples(samplesBuffer,channels,sample_rate)) in
    (*obuf#load_from_samples samplesBuffer channels sample_rate;*)
    while not (osound#get_status = Stopped) do
      Thread.delay 0.01;
      incr i
    done;
    osound#set_buffer obuf;
    osound#play;
    print_int !i;
    print_endline ""
  done;
  f#close
