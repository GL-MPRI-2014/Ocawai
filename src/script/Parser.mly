%token EOF
%token <string> UIDENT
%token <string> LIDENT
%token <int> INT
%token VAR FUN
%token IF ELSE
%token QUOTE SEMICOLON COMMA
%token MOVE ATK MAIN INIT
%token LEFTP RIGHTP
%token LBRACE RBRACE
%token LBRACK RBRACK
%token TRUE FALSE
%token PIP ESP
%token GT LT MUL ADD MIN DIV EQEQ EQUALS NOT

%right PIP ESP
%left GT LT MUL ADD MIN DIV
%left EQUALS EQEQ
%nonassoc NOT

%start <Types.prog_type> file
%%

file:
  | EOF {Types.Empty}
  | d = decl; f = file {Types.Globseq ((d,f),ref `None)}
  | p = proc; f = file {Types.Procseq ((p,f),ref `None)}
  ;

seqexpr:
  |d = decl; s = seqexpr {Types.Seq ((d,s),ref `None)}
  |v = value; SEMICOLON {Types.Return (v,ref `None)}
  |v = value {Types.Return (v,ref `None)}
  ;

decl:
  |VAR; s = LIDENT; EQUALS; v = value; SEMICOLON {Types.Vardecl ((s,v),ref `None)}
  |s = LIDENT; EQUALS; v = value; SEMICOLON {Types.Varset ((s,v),ref `None)}
  |FUN; s = LIDENT; args = strings; EQUALS; LBRACE; e = seqexpr; RBRACE {Types.Fundecl ((s,args,e),ref `None)}
  |FUN; s = LIDENT; LEFTP; RIGHTP; EQUALS; LBRACE; e = seqexpr; RBRACE {Types.Fundecl ((s,[],e),ref `None)}
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
  |v1 = nested_value; PIP; PIP; v2 = nested_value {Types.App (("_or", [v1; v2]),ref `None)}
  |v1 = nested_value; ESP; ESP; v2 = nested_value {Types.App (("_and", [v1; v2]),ref `None)}
  |v1 = nested_value; GT ; v2 = nested_value {Types.App (("_gt", [v1; v2]),ref `None)}
  |v1 = nested_value; LT ; v2 = nested_value {Types.App (("_lt", [v1; v2]),ref `None)}
  |v1 = nested_value; EQEQ ; v2 = nested_value {Types.App (("_eq", [v1; v2]),ref `None)}
  |v1 = nested_value; LT ; EQUALS ; v2 = nested_value {Types.App (("_le", [v1; v2]),ref `None)}
  |v1 = nested_value; GT ; EQUALS ; v2 = nested_value {Types.App (("_ge", [v1; v2]),ref `None)}
  |v1 = nested_value; MUL; v2 = nested_value {Types.App (("_mul", [v1; v2]),ref `None)}
  |v1 = nested_value; ADD; v2 = nested_value {Types.App (("_add", [v1; v2]),ref `None)}
  |v1 = nested_value; MIN; v2 = nested_value {Types.App (("_min", [v1; v2]),ref `None)}
  |v1 = nested_value; DIV; v2 = nested_value {Types.App (("_div", [v1; v2]),ref `None)}
  |NOT; v1 = nested_value {Types.App (("_not", [v1]),ref `None)}
  ;

simple_value:
  |i = INT {Types.Int (i,ref `None)}
  |o = operators {o}
  |LEFTP; RIGHTP {Types.Unit (ref `None)}
  |LEFTP; v1 = value; COMMA; v2 = value; RIGHTP; {Types.Pair ((v1, v2),ref `None)}
  (* Strings with are not well handled yet *)
  |QUOTE; s = LIDENT; QUOTE {Types.String (s,ref `None)}
  |QUOTE; s = UIDENT; QUOTE {Types.String (s,ref `None)}
  |TRUE  {Types.Bool (true,ref `None)}
  |FALSE {Types.Bool (false,ref `None)}
  |LBRACK; v = value_list; RBRACK {Types.List (v,ref `None)}
  |LBRACK; PIP; v = value_list; PIP; RBRACK {Types.Array ((Array.of_list v),ref `None)}
  |s = LIDENT {Types.Var (s,ref `None)}
  ;

composed_value:
  |s = LIDENT; v = values {Types.App ((s,v),ref `None)}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE; ELSE; LBRACE; e = seqexpr; RBRACE {Types.Ifte ((v,t,e),ref `None)}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE {Types.Ifte ((v,t,Types.Return (Types.Unit (ref `None), ref `None)), ref `None)}
  ;

proc:
  |MOVE; s = strings_comma; LBRACE; e = seqexpr; RBRACE {Types.Move ((s,e),ref `None)}
  |ATK;  s = strings_comma; LBRACE; e = seqexpr; RBRACE {Types.Attack ((s,e),ref `None)}
  |MAIN; LBRACE; e = seqexpr; RBRACE {Types.Main (e,ref `None)}
  |INIT; LBRACE; e = seqexpr; RBRACE {Types.Init (e,ref `None)}
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
  |t = value; SEMICOLON; h = value_list {t::h}
  ;
