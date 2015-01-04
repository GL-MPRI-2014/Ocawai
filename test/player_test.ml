open OUnit2
open Player
open Unit
open Position
open List
open Action

let config = let c = new Config.t in c#set_config_name "test_config ";c#init Config.default_config_files;c

let dummy_mutex = Mutex.create ()

(*check empty player*)
let test_player_0 test_ctxt =
  let p = create_dummy_player [] in
  let u = bind (config#unbound_unit "infantry") (create (10,10)) 0 in
  assert_equal ~printer:string_of_int 0 (length (p#get_army))

(*check an army with one unit*)
let test_player_1 test_ctxt =
  let p = create_dummy_player [] in
  let u = bind (config#unbound_unit "infantry") (create (10,10)) 0 in
  p#add_unit u;
    assert_equal ~printer:string_of_int 1 (length (p#get_army))

(*check empty player*)
let test_player_2 test_ctxt =
  let p = create_dummy_player [] in
    assert_equal ~printer:string_of_int 0 (length (p#get_buildings))

(*check delete unit in player*)
let test_player_3 test_ctxt =
  let p = create_dummy_player [] in
  let u =bind (config#unbound_unit "infantry") (create (10,10)) 0 in
  p#add_unit u;
  p#delete_unit (u#get_id);
  assert_equal ~printer:string_of_int 0 (length (p#get_army))



let test_player_4 test_ctxt =
  let p = create_dummy_player [] in
  let u =bind (config#unbound_unit "infantry") (create (10,10)) 0 in
  assert_raises Not_found (fun () -> p#delete_unit (u#get_id);)

(*check empty try_next_action in dummy_player*)
let test_player_5 test_ctxt =
  let p = create_dummy_player [] in
  assert_equal ([], Action.End_turn) (p#get_next_action dummy_mutex)


let suite_player =
  "Dummy player tests">:::
  ["New player : check army">:: test_player_0;
   "New player : check army with one unit">:: test_player_1;
   "New player : check building">:: test_player_2;
   "New player : check delete_unit">:: test_player_3;
   "New player : check failed delete_unit">:: test_player_4;
   "New player : check empty_get_next_action">:: test_player_5]



let () =
  run_test_tt_main suite_player
