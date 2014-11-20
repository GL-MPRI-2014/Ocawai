{
  open Parser

  exception Script_syntax_error of string

  let h_add k e t = Hashtbl.add t k e; t

  let keywords_table = 
    Hashtbl.create 20
    |> h_add "var"      VAR
    |> h_add "if"       IF
    |> h_add "then"     THEN
    |> h_add "else"     ELSE
    |> h_add "move"     MOVE
    |> h_add "attack"   ATK
    |> h_add "main"     MAIN
    |> h_add "init"     INIT
}

let newline = ('\013' * '\010')

let blank = [' ' '\009' '\012']

let integers = ['0'-'9']

let lowercase = ['a'-'z']

let uppercase  = ['A'-'Z']

let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']

let symbolchar = ['>' '<' '=' '*' '+' '-' '/' '&' '|' '!']

rule token = parse
  | blank +
    {token lexbuf}
  | newline
    {token lexbuf}
  | eof 
    {EOF}
  | integers *
    {INT (int_of_string (Lexing.lexeme lexbuf))}
  | uppercase identchar *
    {UIDENT (Lexing.lexeme lexbuf)}
  | lowercase identchar *
    {try Hashtbl.find keywords_table (Lexing.lexeme lexbuf)
     with Not_found -> LIDENT (Lexing.lexeme lexbuf)}
  | "\"" {QUOTE}
  | ";" {SEMICOLON}
  | "=" {EQUALS}
  | "!" {NOT}
  | ">" {GT}
  | "<" {LT}
  | "*" {MUL}
  | "+" {ADD}
  | "-" {MIN}
  | "/" {DIV}
  | "|" {PIP}
  | "&" {ESP}
  | "(" {LEFTP}
  | ")" {RIGHTP}
  | "." {DOT}
  | "," {COMMA}
  | _  {raise (Script_syntax_error ("Syntax Error in script parser : " ^ 
    (Lexing.lexeme lexbuf)))}
