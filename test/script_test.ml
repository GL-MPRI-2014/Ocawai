open OUnit2
open Lexing
open Checker

(* check assoc *)
let test_assoc test_ctxt =
  ScriptCore.init ();
  let f = "test/script_test_assoc.script" in
  let input = open_in f in
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = f};
  try
    let script = Parser.file Lexer.token lexbuf in
    close_in input;
    type_check script [];
    let script = Interpreter.new_script script [] in
    assert_equal (Interpreter.call_f script "test_assoc") (`Bool(true))
  with
    |Lexer.Script_SyntaxError _ ->
        assert_failure "lexing error"
    |Parser.Error ->
        assert_failure "parsing error"

let suite_script =
  "script test">:::
  ["check assoc">:: test_assoc]

let () =
  run_test_tt_main suite_script
