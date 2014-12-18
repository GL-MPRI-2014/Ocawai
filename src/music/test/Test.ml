(**
   Some testing facilities
*)

open Time
open Music
open DList
open TPTM

let dummy_event =
  note (Time.fromInt 1) (new Music.param (C, 4) 127)

let dummy_event_plus (a, b) (pitch, velocity) =
  note (Time.fromPair (a, b)) (new Music.param pitch velocity)

let dummy_simple_event (a, pitch) =
  dummy_event_plus (a, 1) ((pitch, 4), 127)

let dummy_delay : int -> TPTM.t = fun dur ->
  delay (Time.fromInt dur)

(**
   notes : [(int, Music.pitch)], duration and pitch.
*) 
let sequence notes =
  let aggregate tile note =
    tile % (TPTM.make_withDelay (dummy_simple_event note))
  in
  List.fold_left aggregate TPTM.zero notes

let pierrot =
  sequence [(1, C); (1, C); (1, C); (1, D); (2, E); (2, D);
	    (1, C); (1, E); (1, D); (1, D); (4, C)]

let pierrot_canon = fork pierrot ((dummy_delay 8) % pierrot)   

let () =
  print_string "Testing Time module\n";
  print_int (Time.Tempo.tempoToMspq (Num.Int 1));
  print_newline ();

  Format.fprintf Format.std_formatter "@[%a@]@." Time.fprintf (Time.min (Time.inverse bn) wn);
  print_newline ();
  
  let print_TPTM t = TPTM.fprintf Format.std_formatter t; print_newline () in
  
  let myTPTM = normalize pierrot in
  print_TPTM myTPTM;
  TPTM.play pierrot_canon
