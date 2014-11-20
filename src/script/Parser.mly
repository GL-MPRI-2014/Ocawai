%token EOF
%token <string> UIDENT
%token <string> LIDENT
%token <int> INT
%token VAR
%token IF
%token THEN
%token ELSE
%token MOVE
%token ATK
%token MAIN
%token INIT
%token SEMICOLON
%token QUOTE
%token LEFTP
%token RIGHTP
%token DOT
%token COMMA

%right PIP ESP
%left GT LT MUL ADD MIN DIV 
%left EQUALS
%nonassoc NOT
%start <Types.prog_type> file 
%%

file: 
  | {Types.Empty}
  ;
