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
%token GT LT MUL ADD MIN DIV EQUALS NOT

%right PIP ESP
%left GT LT MUL ADD MIN DIV
%nonassoc NOT

%start <Types.prog_type> file
%%

file:
  | EOF {Types.Empty}
  | d = decl; f = file {Types.Globseq (d,f)}
  | p = proc; f = file {Types.Procseq (p,f)}
  ;

seqexpr:
  |d = decl; s = seqexpr {Types.Seq (d,s)}
  |v = value; SEMICOLON {Types.Return v}
  |v = value {Types.Return v}
  ;

decl:
  |VAR; s = LIDENT; EQUALS; v = value; SEMICOLON {Types.Vardecl (s,v)}
  |s = LIDENT; EQUALS; v = value; SEMICOLON {Types.Varset (s,v)}
  |FUN; s = LIDENT; args = strings; EQUALS; LBRACE; e = seqexpr; RBRACE {Types.Fundecl (s,args,e)}
  |FUN; s = LIDENT; LEFTP; RIGHTP; EQUALS; LBRACE; e = seqexpr; RBRACE {Types.Fundecl (s,[],e)}
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
  |v1 = nested_value; PIP; PIP; v2 = nested_value {Types.App ("_or", [v1; v2])}
  |v1 = nested_value; ESP; ESP; v2 = nested_value {Types.App ("_and", [v1; v2])}
  |v1 = nested_value; GT ; v2 = nested_value {Types.App ("_gt", [v1; v2])}
  |v1 = nested_value; LT ; v2 = nested_value {Types.App ("_lt", [v1; v2])}
  |v1 = nested_value; EQUALS ; v2 = nested_value {Types.App ("_eq", [v1; v2])}
  |v1 = nested_value; LT ; EQUALS ; v2 = nested_value {Types.App ("_le", [v1; v2])}
  |v1 = nested_value; GT ; EQUALS ; v2 = nested_value {Types.App ("_ge", [v1; v2])}
  |v1 = nested_value; MUL; v2 = nested_value {Types.App ("_mul", [v1; v2])}
  |v1 = nested_value; ADD; v2 = nested_value {Types.App ("_add", [v1; v2])}
  |v1 = nested_value; MIN; v2 = nested_value {Types.App ("_min", [v1; v2])}
  |v1 = nested_value; DIV; v2 = nested_value {Types.App ("_div", [v1; v2])}
  |NOT; v1 = nested_value {Types.App ("_not", [v1])}
  ;

simple_value:
  |i = INT {Types.Int i}
  |o = operators {o}
  |LEFTP; RIGHTP {Types.Unit}
  (* Strings with are not well handled yet *)
  |QUOTE; s = LIDENT; QUOTE {Types.String s}
  |QUOTE; s = UIDENT; QUOTE {Types.String s}
  |TRUE  {Types.Bool true}
  |FALSE {Types.Bool false}
  |LBRACK; v = value_list; RBRACK {Types.List v}
  |LBRACK; PIP; v = value_list; PIP; RBRACK {Types.Array (Array.of_list v)}
  |s = LIDENT {Types.Var s}
  ;

composed_value:
  |s = LIDENT; v = values {Types.App (s,v)}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE; ELSE; LBRACE; e = seqexpr; RBRACE {Types.Ifte (v,t,e)}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE {Types.Ifte (v,t,Types.Return Types.Unit)}
  ;

proc:
  |MOVE; s = strings_comma; LBRACE; e = seqexpr; RBRACE {Types.Move (s,e)}
  |ATK;  s = strings_comma; LBRACE; e = seqexpr; RBRACE {Types.Attack (s,e)}
  |MAIN; LBRACE; e = seqexpr; RBRACE {Types.Main e}
  |INIT; LBRACE; e = seqexpr; RBRACE {Types.Init e}
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
