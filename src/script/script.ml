open Types
open Lexing
open ScriptCore

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
        Types.Empty
    |Parser.Error ->
        print_position lexbuf;
        Printf.printf " : Syntax Error";
        print_endline "";
        Types.Empty

let parse_file f =
  let input = open_in f in
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = f};
  let script = parse_with_errors lexbuf in
  close_in input;
  script

let () =
  let scr = parse_file "src/script/test.script" in
  if scr <> Types.Empty then print_endline "Script parsed";
  Interpreter.interprete scr
  |> ignore
