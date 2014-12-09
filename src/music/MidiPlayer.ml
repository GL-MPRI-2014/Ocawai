module MusicLog = Log.Make (struct let section = "Music" end)

class asynchronousMidiPlayer =

  let channels = 2 in
  let sample_rate = 44100 in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let mchannels = 16 in
  let mbuf = MIDI.Multitrack.create mchannels blen in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = new Synth.Multitrack.create mchannels (fun _ -> new Synth.square ~adsr sample_rate) in
  let agc = Audio.Effect.auto_gain_control channels sample_rate ~volume_init:0.5 () in
  
  object(self)

    val mutable main_buffer = ref (MIDI.Multitrack.create mchannels 100000)
    val mutable should_run = ref true
    val mutable current_playing = ref 0
    val mutable current_adding = ref 0

    (* This is just blit mapped in every sub channel of a multitrack buf *)
    method private multi_blit b1 o1 b2 o2 len =
      if (MIDI.Multitrack.channels b1 = MIDI.Multitrack.channels b2) then begin
        for i = 0 to (MIDI.Multitrack.channels b1) - 1 do
          MIDI.blit (b1.(i)) o1 (b2.(i)) o2 len
        done
      end else failwith "Wrong number of channels"

    (* This is just add mapped in every sub channel of a multitrack buf *)
    method private multi_add b1 o1 b2 o2 len =
      if (MIDI.Multitrack.channels b1 = MIDI.Multitrack.channels b2) then begin
        for i = 0 to (MIDI.Multitrack.channels b1) - 1 do
          MIDI.add b1.(i) o1 b2.(i) o2 len
        done
      end else failwith "Wrong number of channels"

    method play () =
      let () = MusicLog.infof "Started playing music, created pulseaudio output" in
      let pulse = new MMPulseaudio.writer
                        "OCAWAI"
                        "Music of the OCAWAI game"
                        channels
                        sample_rate in
      while (!should_run) do
        self#multi_blit (!main_buffer) (!current_playing) mbuf 0 blen;
        current_playing := !current_playing + blen;
        synth#play mbuf 0 buf 0 blen;
        agc#process buf 0 blen;
        pulse#write buf 0 blen
      done;
      pulse#close;
      MusicLog.infof "Closed the pulseaudio output"

    method stop () =
      MusicLog.infof "Stoping Music plating";
      should_run := false

    method add new_buffer = 
      self#multi_add (!main_buffer) (!current_adding) new_buffer 0 (MIDI.Multitrack.duration new_buffer);
      current_adding := !current_adding + (MIDI.Multitrack.duration new_buffer)

  end

let play_midi_file fname run =
  let f = new MIDI.IO.Reader.of_file fname in
  let player = new asynchronousMidiPlayer in
  let tmpbuf = MIDI.Multitrack.create 16 1024 in
  let r = ref (f#read 44100 tmpbuf 0 1024) in
  player#add tmpbuf;
  Thread.create (player#play) ();
  while !r <> 0 do
    r := f#read 44100 tmpbuf 0 1024;
    player#add tmpbuf
  done;
  while (!run) do
    Thread.delay 0.1;
  done;
  f#close;
  player#stop ()
