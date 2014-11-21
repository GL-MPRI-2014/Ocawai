open Types

let parse_file f = 
  let input = open_in f in
  let script = 
    Lexing.from_channel input
    |> Parser.file Lexer.token 
  in 
  close_in input;
  script

let () = 
  parse_file "src/script/test.script"
  |> ignore;
  print_endline "Script parsed"
