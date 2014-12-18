{
  open Parser

  exception Script_SyntaxError of string

  let h_add k e t = Hashtbl.add t k e; t

  let keywords_table =
    Hashtbl.create 19 
    |> h_add "var"      VAR
    |> h_add "fun"      FUN
    |> h_add "if"       IF
    |> h_add "else"     ELSE
    |> h_add "move"     MOVE
    |> h_add "attack"   ATK
    |> h_add "main"     MAIN
    |> h_add "init"     INIT
    |> h_add "true"     TRUE
    |> h_add "false"    FALSE

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
    {Lexing.new_line lexbuf; token lexbuf}
  | eof
    {EOF}
  | integers *
    {INT (int_of_string (Lexing.lexeme lexbuf))}
  | lowercase identchar *
    {try Hashtbl.find keywords_table (Lexing.lexeme lexbuf)
     with Not_found -> LIDENT (Lexing.lexeme lexbuf)}
  | "\"" {read_string (Buffer.create 13) lexbuf}
  | ";" {SEMICOLON}
  | "==" {EQEQ}
  | "=" {EQUALS}
  | "!" {NOT}
  | ">" {GT}
  | "<" {LT}
  | "*" {MUL}
  | "+" {ADD}
  | "-" {MIN}
  | "/" {DIV}
  | "||" {PIPPIP}
  | "&&" {ESPESP}
  | "|" {PIP}
  | "(" {LEFTP}
  | ")" {RIGHTP}
  | "," {COMMA}
  | "{" {LBRACE}
  | "}" {RBRACE}
  | "[" {LBRACK}
  | "]" {RBRACK}
  | "//" [ ^ '\r' '\n']* newline
    {Lexing.new_line lexbuf; token lexbuf}
  | _  {raise (Script_SyntaxError ("Syntax Error, unknown char."))}

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/';    read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\';   read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b';   read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n';   read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r';   read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t';   read_string buf lexbuf }
  | newline   { Lexing.new_line lexbuf; read_string buf lexbuf }
  | ['\009' '\012'] { read_string buf lexbuf }
  | [^ '"' '\\' '\r' '\n' '\009' '\012']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _   { raise (Script_SyntaxError ("Illegal string character")) }
  | eof { raise (Script_SyntaxError ("String not terminated")) }
