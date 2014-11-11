open OUnit2
open Player
open Unit
open Position
open List

let test1 test_ctxt = assert_equal "x" "x";;

let test2 test_ctxt = assert_equal 100 100;;

(*let new_player = create_player();;

  let new_unit = create_from_unit_t "name" (create 10 10)*)


(*check empty player*)
let test_player_0 test_ctxt =
  let p = create_player() in
  let u = create_from_config "general" (create (10,10)) in
  assert_equal 0 0
  (*  assert_equal ~printer:string_of_int 0 (length (p#get_army))*)

(*check an army with one unit*)
let test_player_1 test_ctxt =
  let p = create_player() in
  let u = create_from_config "general" (create (10,10)) in
  p#add_unit u;
  assert_equal 0 0

(*check empty player*)
let test_player_2 test_ctxt =
  let p = create_player() in
  let u = create_from_config "general" (create (10,10)) in
  assert_equal 0 0

(*
let test_player_3 =
  let p = create_player() in
  let u = create_from_unit_t "name" (create 10 10) in
  p
  len(p#get_buildings)
*)  
let suite =
  "suite">:::
  ["No unit">:: test_player_0;
   "One unit">:: test_player_1;
   "Zero building">:: test_player_2]


let () =
  run_test_tt_main suite