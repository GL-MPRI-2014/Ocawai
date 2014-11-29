open OcsfmlAudio

let to_s16le c1 c2 =
  let b1 = int_of_char c1 in
  let b2 = int_of_char c2 in
  (b2 * 256) + b1

let my_int_to_string i size =
  let result = Bytes.create size in
  for j = (size) downto 1 do
    Bytes.set
      result
      (j - 1)
      (char_of_int ((i lsr (8 * (j - 1))) mod 256))
  done;
  result


class sound_input_stream = object(self)

  inherit OcsfmlSystem.input_stream as super

  val size = ref 0;
  val f = ref None;
  val cur_pos = ref 0;
  val channels = 1;
  val sample_rate = 44100;
  val bitrate = 16; (*?*)

  method get_size =
    !size

  method tell =
    !cur_pos

  method seek i =
    (-1)

  method read i =
    cur_pos := !cur_pos + i;
    if !cur_pos <= 44 then begin
      match !cur_pos with
        | 12 -> ("RIFF" ^ my_int_to_string 10044 4 ^ "WAVE", 12)
        | 16 -> ("fmt ", 4)
        | 20 -> (my_int_to_string 16 4, 4)
        | 22 -> (my_int_to_string 1 2, 2)
        | 24 -> (my_int_to_string channels 2, 2)
        | 28 -> (my_int_to_string sample_rate 4, 4)
        | 32 -> (my_int_to_string ((sample_rate * bitrate * channels) / 8) 4, 4)
        | 34 -> (my_int_to_string ((bitrate * channels) / 8) 2, 2)
        | 36 -> (my_int_to_string bitrate 2, 2)
        | 40 -> ("data", 4)
        | 44 -> (my_int_to_string 1000000 4, 4)
        | _ -> failwith "-_-"
    end
    else begin
      Printf.printf "Reading -> %d\n%!" i;
      let blen = i in
      let buf = Audio.create channels blen in
      let mchannels = 16 in
      let mbuf = MIDI.Multitrack.create mchannels blen in
      let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
      let synth = new Synth.Multitrack.create mchannels (fun _ -> new Synth.square ~adsr sample_rate) in
      let agc = Audio.Effect.auto_gain_control channels sample_rate ~volume_init:0.3 () in
      ignore (match !f with Some e -> e#read sample_rate mbuf 0 blen | None -> failwith "...");
      synth#play mbuf 0 buf 0 blen;
      agc#process buf 0 blen;
      let strs16le = Audio.S16LE.make buf 0 blen in
      (strs16le, blen)
    end

  method load_file fname =
    f := Some (new MIDI.IO.Reader.of_file fname);
    size := 10000000;
    print_endline "Sound just loaded."

end

let play_midi_file fname run =
  let test = new sound_input_stream in
  test#load_file fname;
  let buf = new sound_buffer (`Stream (test :> OcsfmlSystem.input_stream)) in
  let sound = new sound ~buffer:buf () in
  sound#play;
  while (!run) && (sound#get_status == Playing) do
    Thread.delay 0.01;
  done;
  print_endline "Oh BOY ! You DID reach some place !";
  sound#stop

(*let () =*)
  (*let a = ref true in*)
  (*play_midi_file "./bach_bourree.mid" a*)
