%{
  let loc () = Parsing.(symbol_start_pos (), symbol_end_pos ())
%}


%token EOF
%token <string> LIDENT
%token <string> STRING
%token <int> INT
%token VAR FUN
%token IF ELSE
%token SEMICOLON COMMA
%token MOVE ATK MAIN INIT
%token LEFTP RIGHTP
%token LBRACE RBRACE
%token LBRACK RBRACK
%token TRUE FALSE
%token PIP
%token PIPPIP ESPESP
%token GT LT MUL ADD MIN DIV EQEQ EQUALS NOT

%right PIPPIP ESPESP
%left GT LT ADD MIN
%left DIV MUL
%left EQUALS EQEQ
%nonassoc NOT

%start <ScriptTypes.prog_type> file
%%

file:
  | EOF {ScriptTypes.Empty}
  | d = decl; f = file {ScriptTypes.GlobDecl ((d,f),loc (), ref `None)}
  | p = proc; f = file {ScriptTypes.GlobProc ((p,f),loc (), ref `None)}
  | v = value; SEMICOLON; f = file {ScriptTypes.GlobSeq ((v,f),loc (), ref `None)}
  | v = value; EOF {ScriptTypes.GlobSeq ((v,ScriptTypes.Empty),loc (), ref `None)}
  ;

seqexpr:
  |d = decl; s = seqexpr {ScriptTypes.SeqDecl ((d,s),loc (), ref `None)}
  |v = value; SEMICOLON; s = seqexpr {ScriptTypes.SeqVar ((v,s),loc (), ref `None)}
  |v = value {ScriptTypes.SeqVar ((v,ScriptTypes.SeqEnd), loc (), ref `None)}
  | {ScriptTypes.SeqEnd}
  ;

decl:
  |VAR; s = LIDENT; EQUALS; v = value; SEMICOLON {ScriptTypes.Vardecl ((s,v), loc (), ref `None)}
  |s = LIDENT; EQUALS; v = value; SEMICOLON {ScriptTypes.Varset ((s,v), loc (), ref `None)}
  |FUN; s = LIDENT; args = strings; EQUALS; LBRACE; e = seqexpr; RBRACE 
    {ScriptTypes.Fundecl ((s,args,e), loc (),ref `None)}
  |FUN; s = LIDENT; LEFTP; RIGHTP; EQUALS; LBRACE; e = seqexpr; RBRACE
    {ScriptTypes.Fundecl ((s,[],e),loc (), ref `None)}
  ;

value:
  |c = composed_value {c}
  |n = nested_value {n}
  ;

nested_value:
  |s = simple_value {s}
  |LEFTP; s = simple_value; RIGHTP {s}
  |LEFTP; c = composed_value; RIGHTP {c}
  ;

operators:
  |v1 = nested_value; PIPPIP; v2 = nested_value 
    {ScriptTypes.App (("_or", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; ESPESP; v2 = nested_value 
    {ScriptTypes.App (("_and", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; GT ; v2 = nested_value 
    {ScriptTypes.App (("_gt", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; LT ; v2 = nested_value 
    {ScriptTypes.App (("_lt", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; EQEQ ; v2 = nested_value 
    {ScriptTypes.App (("_eq", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; LT ; EQUALS ; v2 = nested_value 
    {ScriptTypes.App (("_le", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; GT ; EQUALS ; v2 = nested_value 
    {ScriptTypes.App (("_ge", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; MUL; v2 = nested_value 
    {ScriptTypes.App (("_mul", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; ADD; v2 = nested_value 
    {ScriptTypes.App (("_add", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; MIN; v2 = nested_value 
    {ScriptTypes.App (("_sub", [v1; v2]), loc (), ref `None)}
  |v1 = nested_value; DIV; v2 = nested_value 
    {ScriptTypes.App (("_div", [v1; v2]), loc (), ref `None)}
  |NOT; v1 = nested_value {ScriptTypes.App (("_not", [v1]), loc (), ref `None)}
  ;

simple_value:
  |i = INT {ScriptTypes.Int (i, loc (), ref `None)}
  |o = operators {o}
  |LEFTP; RIGHTP {ScriptTypes.Unit ( loc (), ref `None)}
  |LEFTP; v1 = value; COMMA; v2 = value; RIGHTP; {ScriptTypes.Pair ((v1, v2), loc (), ref `None)}
  |s = STRING {ScriptTypes.String (s, loc (), ref `None)}
  |TRUE  {ScriptTypes.Bool (true, loc (), ref `None)}
  |FALSE {ScriptTypes.Bool (false, loc (), ref `None)}
  |LBRACK; v = value_list; RBRACK {ScriptTypes.List (v, loc (), ref `None)}
  |LBRACK; PIP; v = value_list; PIP; RBRACK {ScriptTypes.Array ((Array.of_list v), loc (), ref `None)}
  |s = LIDENT {ScriptTypes.Var (s, loc (), ref `None)}
  ;

composed_value:
  |s = LIDENT; v = values {ScriptTypes.App ((s,v), loc (), ref `None)}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE; ELSE; LBRACE; e = seqexpr; RBRACE 
    {ScriptTypes.Ifte ((v,t,e), loc (), ref `None)}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE 
    {ScriptTypes.Ifte ((v,t,ScriptTypes.SeqEnd),  loc (), ref `None)}
  ;

proc:
  |MOVE; s = strings_comma; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Move ((s,e), loc (), ref `None)}
  |ATK;  s = strings_comma; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Attack ((s,e), loc (), ref `None)}
  |MAIN; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Main (e, loc (), ref `None)}
  |INIT; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Init (e, loc (), ref `None)}
  ;

values:
  |t = nested_value {[t]}
  |t = nested_value; h = values {t::h}
  ;

strings:
  |s = LIDENT {[s]}
  |s = LIDENT; t = strings {s::t}
  ;

strings_comma:
  |s = LIDENT {[s]}
  |s = LIDENT; COMMA; t = strings_comma {s::t}
  ;

value_list:
  | {[]}
  |t = value {[t]}
  |t = value; SEMICOLON; h = value_list {t::h}
  ;