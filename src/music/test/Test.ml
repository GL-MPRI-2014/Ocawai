(**
   Some testing facilities
*)

open Time
open Music
open DList
open TPTM

let dummy_event (a, b) =
  note (Time.fromPair (a, b)) (new Music.param (C, 4) 127)

let dummy_sequence : TPTM.t list =
  List.map TPTM.make_withDelay (List.map dummy_event [(1,1); (1, 2); (1, 1)])

let parallelProd =
  TPTM.fromList dummy_sequence

let () =
  Format.fprintf Format.std_formatter "@[%a@]@." Time.fprintf (Time.min (Time.inverse bn) wn);
  print_newline ();
  let print_TPTM t = TPTM.fprintf Format.std_formatter t; print_newline () in
  print_TPTM (
      delay (Time.fromPair (-1, 1)) % (TPTM.make (dummy_event (1, 1)))
    );
  print_TPTM (delay (Time.fromPair (-1, 1)) % parallelProd)

