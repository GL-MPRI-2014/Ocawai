open OUnit2
open Unit

let config = let c = new Config.t in c#set_config_name "test_config ";c#init Config.default_config_files;c

(*basic move*)
let test_logics_move_1 test_ctxt =
  let p1 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p1id = p1#get_id in
  let m = Battlefield.create 10 10 (Config.config#tile "plain") in
  let u1 = bind (config#unbound_unit "test1") (Position.create (4,4)) p1id in
  p1#add_unit u1;
  let (_, h) = Logics.accessible_positions u1 p1 [p1] m in
  assert_equal true (Hashtbl.mem h (Position.create (8,4)))

(*blocked by water*)
let test_logics_move_2 test_ctxt =
  let p1 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p1id = p1#get_id in
  let m = Battlefield.create 10 10 (Config.config#tile "plain") in
  Battlefield.set_tile m (Position.create (5,4)) (Config.config#tile "water");
  Battlefield.set_tile m (Position.create (4,5)) (Config.config#tile "water");
  Battlefield.set_tile m (Position.create (3,4)) (Config.config#tile "water");
  Battlefield.set_tile m (Position.create (4,3)) (Config.config#tile "water");
  let u1 = bind (config#unbound_unit "test1") (Position.create (4,4)) p1id in
  p1#add_unit u1;
  let (_, h) = Logics.accessible_positions u1 p1 [p1] m in
  assert_equal true (not (Hashtbl.mem h (Position.create (8,4))))

(*move through ally unit*)
let test_logics_move_3 test_ctxt =
  let p1 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p1id = p1#get_id in
  let m = Battlefield.create 10 10 (Config.config#tile "plain") in
  let u1 = bind (config#unbound_unit "test1") (Position.create (4,4)) p1id in
  let u2 = bind (config#unbound_unit "test1") (Position.create (6,4)) p1id in
  p1#add_unit u1;
  p1#add_unit u2;
  let (_, h) = Logics.accessible_positions u1 p1 [p1] m in
  assert_equal true (Hashtbl.mem h (Position.create (8,4)))

(*blocked by enemy unit*)
let test_logics_move_4 test_ctxt =
  let p1 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p1id = p1#get_id in
  let p2 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p2id = p2#get_id in
  let m = Battlefield.create 10 10 (Config.config#tile "plain") in
  let u1 = bind (config#unbound_unit "test1") (Position.create (4,4)) p1id in
  p1#add_unit u1;
  let u2 = bind (config#unbound_unit "test1") (Position.create (7,4)) p2id in
  let u3 = bind (config#unbound_unit "test1") (Position.create (8,3)) p2id in
  let u4 = bind (config#unbound_unit "test1") (Position.create (8,5)) p2id in
  let u5 = bind (config#unbound_unit "test1") (Position.create (9,4)) p2id in
  p2#set_army [u2; u3; u4; u5];
  let (_, h) = Logics.accessible_positions u1 p1 [p1; p2] m in
  assert_equal true (not (Hashtbl.mem h (Position.create (8,4))))

(*same situation, but no vision on enemy*)
let test_logics_move_5 test_ctxt =
  let p1 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p1id = p1#get_id in
  let p2 = (Player.create_dummy_player [] :> Player.logicPlayer) in
  let p2id = p2#get_id in
  let m = Battlefield.create 10 10 (Config.config#tile "plain") in
  let u1 = bind (config#unbound_unit "test2") (Position.create (4,4)) p1id in
  p1#add_unit u1;
  let u2 = bind (config#unbound_unit "test1") (Position.create (7,4)) p2id in
  let u3 = bind (config#unbound_unit "test1") (Position.create (8,3)) p2id in
  let u4 = bind (config#unbound_unit "test1") (Position.create (8,5)) p2id in
  let u5 = bind (config#unbound_unit "test1") (Position.create (9,4)) p2id in
  p2#set_army [u2; u3; u4; u5];
  let (_, h) = Logics.accessible_positions u1 p1 [p1; p2] m in
  assert_equal true (Hashtbl.mem h (Position.create (8,4)))


let suite_logics_move =
  "Movement tests">:::
    ["Basic movement">:: test_logics_move_1;
     "Blocked by terrain">:: test_logics_move_2;
     "Movement through ally">:: test_logics_move_3;
     "Blocked by enemy unit">:: test_logics_move_4;
     "Movement throuh enemy with no vision">:: test_logics_move_5
    ]


let () =
  run_test_tt_main suite_logics_move
