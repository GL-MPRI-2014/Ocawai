open OUnit2

(* check assoc *)
let test_assoc test_ctxt =
  let script = ScriptEngine.script_from_file "script_test_assoc.script" [] [] in
  assert_equal (Interpreter.call_f script "test_assoc") (`Bool(true))

let suite_script =
  "script test">:::
  ["check assoc">:: test_assoc]

let () =
  run_test_tt_main suite_script
