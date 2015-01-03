(**
   Some testing facilities
*)

open Time
open Music
open DList
open TPTM

let dummy_event =
  note (Time.fromInt 1) (new Music.param (C, 4) 127)

let dummy_event_plus dur (pitch, velocity) =
  note dur (new Music.param pitch velocity)

let dummy_simple_event (dur, pitch) =
  dummy_event_plus dur ((pitch, 4), 127)

let dummy_simple_event_pitchclass (dur, pitchClass) =
  dummy_event_plus dur (pitchClass, 127)


(**
   notes : [(int, Music.pitch)], duration and pitch, all on octave 4.
*) 
let sequence notes =
  let aggregate tile note =
    tile % (TPTM.make_withDelay (dummy_simple_event note))
  in
  List.fold_left aggregate TPTM.zero notes

let pierrot =
  sequence [(en, C); (en, C); (en, C); (en, D); (qn, E); (qn, D);
	    (en, C); (en, E); (en, D); (en, D); (hn, C)]

let pierrot_canon = fork pierrot @@ (delay wn) % pierrot   
						   
let first_measure = fst @@ extract_by_time bn pierrot_canon

let music_player =
  fun ?samplerate:(samplerate = MidiV.samplerate) ?division:(division = MidiV.division)
      ?tempo:(tempo = Time.Tempo.base) () ->
  object (self)
	   
    (** The main Tile we will use to store the music to play,
        we maintain Pre buffer = Pos buffer = O *)
    val mutable buffer = TPTM.zero
				  
    method bufferize : TPTM.t -> unit = fun t ->
      let open TPTM in
      buffer <- reset @@ buffer % t
				    
    method play_next_measure : unit -> unit = fun () ->
      let minisleep (sec: float) =
	ignore (Unix.select [] [] [] sec)
      and one_measure =
	let duration_seconds_num =
	  Num.mult_num (Num.num_of_int @@ MidiV.timeToSamplesNumber Time.wn)
		       (Num.div_num (Num.num_of_int 1) (Num.num_of_int samplerate))
	in
        Num.float_of_num duration_seconds_num
      in
      while true do
	let (next_measure, rest) =
	  TPTM.extract_by_time wn buffer
	in
	buffer <- reset rest;
	TPTM.fork_play next_measure;
	minisleep (one_measure)
      done

    method read_note : unit =
      print_endline "\n\027[32mInput some notes (for instance C5 A4 A3 Aff2), \
		     then press ENTER\027[0m";
      print_endline "\027[1;31m\t=======> Be amazed ! <=======\027[0m\n\
		     (You can even let it run in the background, \
		     it won't eat all your CPU-time :) !)\n";
      while true do
	try
	  let notes_strings = Str.split (Str.regexp " ") (read_line ()) in
	  let note_reader = fun str ->
	    TPTM.make_withDelay @@ dummy_simple_event_pitchclass @@
	      (wn, Music.pitch_of_string str)
	  in 
	  let notes = List.map note_reader notes_strings in
	  List.iter self#bufferize notes
	with Not_found -> ()
      done
  end

let () =
  (*
  print_string "Testing Time module\n";
  print_int (Time.Tempo.toMicrosecondsPerQuarters (Time.Tempo.base));
  print_newline ();

  Format.fprintf Format.std_formatter "@[%a@]@." Time.fprintf (Time.min (Time.inverse bn) wn);
  print_newline ();
  
  let print_TPTM t = TPTM.fprintf Format.std_formatter t; print_newline () in
  
  let myTPTM = normalize pierrot in
  print_TPTM myTPTM; 
  TPTM.play ~tempo:(Time.Tempo.fromInt 100) pierrot_canon;
  Unix.sleep 3; 
  print_TPTM first_measure;
  TPTM.fork_play ~tempo:(Time.Tempo.fromInt 100) first_measure;
  Thread.delay 10. *)
  print_endline "Starting the big test !";
  let my_music_player = music_player () in
  ignore @@ Thread.create my_music_player#play_next_measure (); 
  my_music_player#read_note
