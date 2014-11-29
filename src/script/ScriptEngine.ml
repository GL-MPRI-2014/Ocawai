open ScriptTypes
open Lexing
open Checker
open Interpreter

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let str = Lexing.lexeme lexbuf in
  let begchar = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.printf "In %s, line %d, characters %d-%d : %s"
    pos.pos_fname pos.pos_lnum begchar
    (begchar + (String.length str))
    (Lexing.lexeme lexbuf)

let parse_with_errors lexbuf =
  try
    Parser.file Lexer.token lexbuf
  with
    |Lexer.Script_SyntaxError msg ->
        print_position lexbuf;
        Printf.printf " : %s" msg;
        print_endline "";
        ScriptTypes.Empty
    |Parser.Error ->
        print_position lexbuf;
        Printf.printf " : Syntax Error";
        print_endline "";
        ScriptTypes.Empty

let script_from_file f =
  let input = open_in f in
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = f};
  let script = parse_with_errors lexbuf in
  close_in input;
  type_check script ;
  Interpreter.new_script script

let () = ScriptCore.init ()
