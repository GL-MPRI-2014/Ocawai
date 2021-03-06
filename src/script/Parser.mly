%token EOF
%token <string> LIDENT
%token <string> STRING
%token <int> INT
%token VAR FUN
%token IF ELSE
%token SEMICOLON COMMA
%token MOVE ATK MAIN INIT BUILD
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
  | d = decl; f = file {ScriptTypes.GlobDecl ((d,f),($startpos(d),$endpos(d)))}
  | p = proc; f = file {ScriptTypes.GlobProc ((p,f),($startpos(p),$endpos(p)))}
  | v = value; SEMICOLON; f = file {ScriptTypes.GlobSeq ((v,f),($startpos(v),$endpos(v)))}
  | v = value; EOF {ScriptTypes.GlobSeq ((v,ScriptTypes.Empty),($startpos(v),$endpos(v)))}
  ;

seqexpr:
  |d = decl; s = seqexpr {ScriptTypes.SeqDecl ((d,s),($startpos(d),$endpos(d)))}
  |v = value; SEMICOLON; s = seqexpr {ScriptTypes.SeqVar ((v,s),($startpos(v),$endpos(v)))}
  |v = value {ScriptTypes.SeqVar ((v,ScriptTypes.SeqEnd), ($startpos(v),$endpos(v)))}
  | {ScriptTypes.SeqEnd}
  ;

decl:
  |VAR; s = LIDENT; EQUALS; v = value; SEMICOLON {ScriptTypes.Vardecl ((s,v), ($startpos,$endpos))}
  |s = LIDENT; EQUALS; v = value; SEMICOLON {ScriptTypes.Varset ((s,v), ($startpos,$endpos))}
  |FUN; s = LIDENT; args = strings; EQUALS; LBRACE; e = seqexpr; RBRACE
    {ScriptTypes.Fundecl ((s,args,e), ($startpos(s),$startpos(e)))}
  |FUN; s = LIDENT; LEFTP; RIGHTP; EQUALS; LBRACE; e = seqexpr; RBRACE
    {ScriptTypes.Fundecl ((s,[],e),($startpos(s),$startpos(e)))}
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
    {ScriptTypes.App (("_or", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; ESPESP; v2 = nested_value
    {ScriptTypes.App (("_and", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; GT ; v2 = nested_value
    {ScriptTypes.App (("_gt", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; LT ; v2 = nested_value
    {ScriptTypes.App (("_lt", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; EQEQ ; v2 = nested_value
    {ScriptTypes.App (("_eq", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; LT ; EQUALS ; v2 = nested_value
    {ScriptTypes.App (("_le", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; GT ; EQUALS ; v2 = nested_value
    {ScriptTypes.App (("_ge", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; MUL; v2 = nested_value
    {ScriptTypes.App (("_mul", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; ADD; v2 = nested_value
    {ScriptTypes.App (("_add", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; MIN; v2 = nested_value
    {ScriptTypes.App (("_sub", [v1; v2]), ($startpos,$endpos))}
  |v1 = nested_value; DIV; v2 = nested_value
    {ScriptTypes.App (("_div", [v1; v2]), ($startpos,$endpos))}
  |NOT; v1 = nested_value {ScriptTypes.App (("_not", [v1]), ($startpos,$endpos))}
  ;

simple_value:
  |i = INT {ScriptTypes.Int (i, ($startpos,$endpos))}
  |o = operators {o}
  |LEFTP; RIGHTP {ScriptTypes.Unit ( ($startpos,$endpos))}
  |LEFTP; v1 = value; COMMA; v2 = value; RIGHTP; {ScriptTypes.Pair ((v1, v2), ($startpos,$endpos))}
  |s = STRING {ScriptTypes.String (s, ($startpos,$endpos))}
  |TRUE  {ScriptTypes.Bool (true, ($startpos,$endpos))}
  |FALSE {ScriptTypes.Bool (false, ($startpos,$endpos))}
  |LBRACK; v = value_list; RBRACK {ScriptTypes.List (v, ($startpos,$endpos))}
  |LBRACK; PIP; v = value_list; PIP; RBRACK {ScriptTypes.Array ((Array.of_list v), ($startpos,$endpos))}
  |s = LIDENT {ScriptTypes.Var (s, ($startpos,$endpos))}
  ;

composed_value:
  |s = LIDENT; v = values {ScriptTypes.App ((s,v), ($startpos,$endpos))}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE; ELSE; LBRACE; e = seqexpr; RBRACE
    {ScriptTypes.Ifte ((v,t,e), ($startpos,$endpos))}
  |IF; LEFTP; v = value; RIGHTP; LBRACE; t = seqexpr; RBRACE
    {ScriptTypes.Ifte ((v,t,ScriptTypes.SeqEnd),  ($startpos,$endpos))}
  ;

proc:
  |MOVE; s = strings_comma; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Move ((s,e), ($startpos(s),$endpos(s)))}
  |BUILD; s = strings_comma; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Build ((s,e), ($startpos(s),$endpos(s)))}
  |ATK;  s = strings_comma; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Attack ((s,e), ($startpos(s),$endpos(s)))}
  |MAIN; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Main (e, ($startpos(e),$endpos(e)))}
  |INIT; LBRACE; e = seqexpr; RBRACE {ScriptTypes.Init (e, ($startpos(e),$endpos(e)))}
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
