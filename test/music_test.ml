(**
   Testing modules related to the generation of music
*)

open OUnit2

(*
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
 *)

let () =
  Random.self_init ()

let my_rel_cmp : epsilon:float -> int -> int -> bool = fun ~epsilon expected real ->
  (** Rounds should be "by above" *) 
  (expected <= real) &&
    let expected_float = float_of_int expected
    and real_float     = float_of_int real in
    OUnit2.cmp_float ~epsilon expected_float real_float

(** Module Time *)

let timePrinter : Time.t -> string = fun t ->
  Time.fprintf Format.str_formatter t;
  Format.flush_str_formatter ()

let time_isEqual t1 t2 =
  Time.compare t1 t2 = 0
let time_isStrictlyGreater t1 t2 =
  Time.compare t1 t2 = +1
let time_isStrictlyLess t1 t2 =
  Time.compare t1 t2 = -1

(** Test group operations over Time.t *)
let test_time_1 test_ctxt =
  let open Time in
  let ( /+/ ) = Time.plus
  and ( /-/ ) = Time.minus in
  let one_measure = wn
  and sum = hn /+/ hn
  and sum_and_minus = wn /+/ qn /+/ qn /-/ hn
  in
  let my_assert = assert_equal ~printer:timePrinter ~cmp:time_isEqual in
  my_assert ~msg:"Trivial equality" one_measure one_measure;
  my_assert ~msg:"Simple sum" one_measure sum;
  my_assert ~msg:"Composite sum" one_measure sum_and_minus

(** Testing Time.t equality with non normal forms *)
let test_time_2 test_ctxt =
  let open Time in
  let normal_form = fromPair (1, 2)
  and non_normal = fromPair (2, 4) in
  let my_assert = assert_equal ~printer:timePrinter ~cmp:time_isEqual in
  my_assert ~msg:"Direct equality test" normal_form non_normal;
  my_assert ~msg:"Substract then compare to Time.zero"
	        Time.zero @@ Time.minus normal_form non_normal
	       
(** Test Time.t comparison *)
let test_time_3 test_ctxt =
  let open Time in
  let time_positive = qn
  and time_negative = inverse qn in
  let assert_less = assert_equal ~cmp:time_isStrictlyLess ~printer:timePrinter in
  assert_less ~msg:"First value should be less than second"
	      time_negative time_positive;
  assert_bool "Negative time should NOT be greater than positive time" @@
    not (time_isStrictlyGreater time_negative time_positive) 

(** Test Time.t to MIDI ticks conversion *)
let test_time_4 test_ctxt =
  let open Time in
  let duration = wn in
  let random_int = Random.int 300 in
  let with_default = Time.toMidiTicks ~division:(MidiV.division) duration
  and with_random =
    Time.toMidiTicks ~division:(MIDI.Ticks_per_quarter random_int) duration
  in
  assert_equal ~printer:string_of_int
	       ~msg:"Testing with default MIDI division value"
	       (4*96) @@ with_default;  
  assert_equal ~printer:string_of_int
	       ~msg:"Testing with random MIDI division value"
	       (4*random_int) @@ with_random

(** Test Time.Tempo.t to Microseconds per quarter conversion *)
let test_time_5 test_ctxt =
  let open Time in
  let random_int = Random.int 300 in
  let random_tempo = Tempo.fromInt random_int
  and random_manual_conv =
    let float = float_of_int random_int in
    int_of_float @@ (1. /. float) *. 60. *. (10.**6.)
  in
  assert_equal ~printer:string_of_int
	       ~msg:"Converting default tempo, 120 BPM"
	       500000 @@ Tempo.toMicrosecondsPerQuarters Tempo.base;
  assert_equal ~cmp:(my_rel_cmp ~epsilon:0.0001)
	       ~printer:string_of_int
	       ~msg:(Printf.sprintf "Converting random tempo, %d BPM\n\
				     Result should be greater, at most by 2 than expected value"
				    random_int)
	       random_manual_conv @@ Tempo.toMicrosecondsPerQuarters random_tempo
  
let suite_music_time =
  "Music : Time module tests">:::
    ["Group / arithmetic operations testing">::test_time_1;
     "Time.t equality testing">::test_time_2;
     "Time.t comparison testing">::test_time_3;
     "Time.t to MIDI ticks conversion testing">:: test_time_4;
     "Time.Tempo.t to Microseconds per quarter conversion testing">::test_time_5
    ]

let () =
  run_test_tt_main suite_music_time

(** Midi Values module tests *)

(** Test Time.t to samples number conversion *)  
let test_midiv_1 test_ctxt =
  let open Time in
  let open MidiV in
  (** Reference time : one whole-note == une ronde *)
  let duration = wn in
  let expected_manual_default = 88200 in
  let my_assert_equal_rel = assert_equal ~cmp:(my_rel_cmp ~epsilon:0.0001) in
  my_assert_equal_rel ~printer:string_of_int
		      ~msg:"Testing with default parameters"
		      88200 @@ MidiV.timeToSamplesNumber duration;
  let samplerate = 96000
  and division = MIDI.Ticks_per_quarter 184
  and tempo = Tempo.fromInt 180 in
  my_assert_equal_rel ~printer:string_of_int 
		      ~msg:"Testing with arbitrary parameters"
		      128000 @@
    MidiV.timeToSamplesNumber ~samplerate ~division
			      ~tempo ~duration

let suite_music_midiv =
  "Music : MidiV module tests">:::
    ["Time.t to samples number conversion">::test_midiv_1;
    ]

let () =
  run_test_tt_main suite_music_midiv

(** Music module tests *)

let musicEvent_to_string t =
  Music.fprintf Format.str_formatter t;
  Format.flush_str_formatter ()

(** Testing string to pitch conversion *)
let test_music_1 test_ctxt =
  let open Music in
  let pitch_ref = (Bss, 7)
  and pitch_read = pitch_of_string "Bss7"
  in
  assert_equal ~printer:pitch_to_string
	       ~msg:"Testing manual, correct input"
	       pitch_ref pitch_read;
  let read_incorrect_input () = pitch_of_string "Asz" in
  assert_raises ~msg:"Testing exception with incorrect pitch input \"Asz\""
		(Music.Not_found) read_incorrect_input

(** Testing correction of note creation guards *)
let test_music_2 test_ctxt =
  let open Music in
  let dur = Time.hn in
  let neg_dur = Time.inverse dur in
  let param = new param (A,4) 127 in
  let create_negative_note () =
    note neg_dur param
  in
  assert_raises ~msg:"Testing failure of creation of negative duration note" 
		Music.Negative_duration_note
		create_negative_note
		

(** Testing (dummy) event comparison (the [Music.compare] does not make
    much sense semantically, using [Pervasives.compare] on note parameters,
    but should at least never raise an exception) *) 
let test_music_3 test_ctxt =
  let open Music in
  let dur = Time.hn in
  let neg_dur = Time.inverse dur in
  let pos_rest = rest dur
  and neg_rest = rest neg_dur in
  let assert_less =
    assert_equal ~printer:musicEvent_to_string
		 ~cmp:(fun e1 e2 -> Music.compare e1 e2 = -1)
  in
  assert_less ~msg:"Testing [Rest dur] comparison,
		       result should be greater than expected value"
		 neg_rest pos_rest;
  let param = new param (A,4) 127 in
  let note_event = note dur param in 
  assert_less ~msg:"Testing comparison of [Rest dur] with [Note (dur, param)]"
	      pos_rest note_event
		
let suite_music_music =
  "Music : Music module tests">:::
    ["String to pitch conversion testing">::test_music_1;
     "Note creation testing">::test_music_2;
     "'a Music.t comparison testing">::test_music_3;
    ]

let () =
  run_test_tt_main suite_music_music
