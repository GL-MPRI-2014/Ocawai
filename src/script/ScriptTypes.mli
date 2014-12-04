(** Basic types for the script parser and interpreter *)

(** Type of locations in the script *)
type location = Lexing.position * Lexing.position

(** Type of procedures called by the interpreter *)
type procedure_type =
    Move   of (string list * seq_type) * location
  | Attack of (string list * seq_type) * location
  | Main of seq_type * location
  | Init of seq_type * location

(** Type of the values manipulated by the AIs *)
and values_type =
    Int     of int * location
  | Unit    of location
  | String  of string * location
  | Bool    of bool * location
  | List    of values_type list * location
  | Array   of values_type array * location
  | Var     of string * location
  | App     of (string * values_type list) * location
  | Ifte    of (values_type * seq_type * seq_type) * location
  | Pair    of (values_type * values_type) * location

(** Type of a variable/function declaration *)
and decl_type =
    Vardecl of (string * values_type) * location
  | Varset  of (string * values_type) * location
  | Fundecl of (string * string list * seq_type) * location

(** Type of the sequences (body of a function) *)
and seq_type =
    SeqDecl of (decl_type * seq_type) * location
  | SeqVar  of (values_type * seq_type) * location
  | SeqEnd

(** Type of a program *)
and prog_type =
    GlobDecl of (decl_type * prog_type) * location
  | GlobProc of (procedure_type * prog_type) * location
  | GlobSeq  of (values_type * prog_type) * location
  | Empty


(** Unifiable types for the type-checker *)
and static = [
  `Int_tc     |
  `Unit_tc    |
  `String_tc  |
  `Bool_tc    |
  `Soldier_tc |
  `Map_tc     |
  `Player_tc  |
  `List_tc  of term_type |
  `Array_tc of term_type |
  `Fun_tc   of term_type * term_type |
  `Pair_tc  of term_type * term_type |
  `Alpha_tc of int       |
  `None       |
  `Pointer  of term_type
]

and term_type = static ref
