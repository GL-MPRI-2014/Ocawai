(*open OUnit2*)

(*open Manager*)

(*let test_snake test_ctxt =*)
  (*(new Snake.state :> State.state) |> manager#push ;*)
   (*Cannot run it if we cannot stop it *)
   (*manager#run; *)
  (*assert_equal () ()*)

(*let suite_interface =*)
  (*"Interface tests" >::: [*)
    (*"Snake testing" >:: test_snake*)
  (*]*)

(*let () =*)
  (*run_test_tt_main suite_interface*)
