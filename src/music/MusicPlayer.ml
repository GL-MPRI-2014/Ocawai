(**
   Music reading and playback object
*)

open Time
open Music
open DList
open TPTM

let dummy_event_plus dur (pitch, velocity) =
  note dur (new Music.param pitch velocity)

let dummy_simple_event (dur, pitch) =
  dummy_event_plus dur (pitch, 127)

(**
   notes : [(int, Music.pitch)], duration and pitch, all on octave 4.
*) 
let sequence notes =
  let aggregate tile note =
    tile % (TPTM.make_withDelay (dummy_simple_event note))
  in
  List.fold_left aggregate TPTM.zero notes

let chord duration pitches =
  let aggregate tile pitch =
    fork tile @@ TPTM.make_withDelay (dummy_simple_event (duration, pitch))
  in
  List.fold_left aggregate TPTM.zero pitches
 
let menu_music = (reset @@ sequence [(hn, (C, 3));
				     (hn, (E, 3))]) %
		   (sequence [(hn, (G, 3)); (hn, (B, 4))])

let winner_music = chord wn @@ [(C, 4); (E, 4); (G, 4)]

let loser_music = chord wn @@ [(C, 4); (Ef, 4); (G, 4)]

let music_player =
  fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
      ?tempo:(tempo = Time.Tempo.base) () ->
  object (self)
	   
    (** The main Tile we will use to store the music to play,
        we maintain Pre buffer = Pos buffer = O *)
    val mutable buffer = TPTM.zero

    method private duration_one_measure tempo =
      let duration_seconds_num =
        Num.mult_num (Num.num_of_int @@ MidiV.timeToSamplesNumber Time.wn)
                     (Num.div_num (Num.num_of_int 1) (Num.num_of_int samplerate))
      in
      Num.float_of_num duration_seconds_num
				  
    method private bufferize : TPTM.t -> unit = fun t ->
      let open TPTM in
      buffer <- reset @@ buffer % t

    method play_menu : bool ref -> unit = fun run ->
      let tempo = Time.Tempo.fromInt 100 in
      let midi_player = new MidiPlayer.asynchronousMidiPlayer in
      ignore @@ Thread.create (self#add_next_measure tempo) midi_player;
      ignore @@ Thread.create (midi_player#play) ();
      while true do
        while !run do
          self#bufferize menu_music;
          Thread.delay (self#duration_one_measure tempo)
        done;
        Thread.delay 0.1;
      done

    method play_game : bool ref -> unit = fun run ->
      let tempo = Time.Tempo.fromInt 100 in
      let midi_player = new MidiPlayer.asynchronousMidiPlayer in
      ignore @@ Thread.create (self#add_next_measure tempo) midi_player;
      ignore @@ Thread.create (midi_player#play) ();
      while true do
	while !run do
	  let mood = Mood.get () in
	  let next_tile =
	    if mood <= 0. then
	      loser_music
	    else winner_music
	  in
	  self#bufferize next_tile;
          Thread.delay (self#duration_one_measure tempo)
	done;
        Thread.delay 0.1;
      done

    method private pick_measure : unit -> unit = fun () ->
      (** Get current mood and pick a measure to play *)
      (* TODO, right now
       let mood = Mood.get () in *)()
      
    method add_next_measure = fun tempo midi_player ->
      while true do
        let (next_measure, rest) =
          TPTM.extract_by_time wn buffer
        in
        buffer <- reset rest;
        let new_buffer = TPTM.to_MIDI_buffer ~tempo next_measure in
	midi_player#add new_buffer;
        Thread.delay (self#duration_one_measure tempo)
      done

    method read_note : unit =
      let running = ref true in
      print_endline "\n\027[32mInput some notes (for instance C5 A4 A3 Aff2), \
		     then press ENTER\027[0m (Input <q> to quit)";
      print_endline "\027[1;31m\t=======> Be amazed ! <=======\027[0m\n\
		     (You can even let it run in the background, \
		     it won't eat all your CPU-time :) !)\n";
      while !running do
        let notes_strings = Str.split (Str.regexp " ") (read_line ()) in
	try
          let note_reader = fun str ->
            TPTM.make_withDelay @@ dummy_simple_event @@
              (wn, Music.pitch_of_string str)
          in 
          let notes = List.map note_reader notes_strings in
          List.iter self#bufferize notes
        with Not_found ->
	     match notes_strings with
	     | ["q"] -> running := false
	     | _ -> ()
      done

    initializer
      Random.self_init ()
  end
