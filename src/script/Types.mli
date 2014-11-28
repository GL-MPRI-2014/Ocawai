(** Basic types for the script parser and interpreter *)

(** Type of locations in the script *)
type location = Lexing.position * Lexing.position

(** Type of procedures called by the interpreter *)
type procedure_type =
    Move   of (string list * seq_type) * location * term_type
  | Attack of (string list * seq_type) * location * term_type
  | Main of seq_type * location * term_type
  | Init of seq_type * location * term_type

(** Type of the values manipulated by the AIs *)
and values_type =
    Int     of int * location * term_type
  | Unit    of location * term_type
  | String  of string * location * term_type
  | Bool    of bool * location * term_type
  | List    of values_type list * location * term_type
  | Array   of values_type array * location * term_type
  | Var     of string * location * term_type
  | App     of (string * values_type list) * location * term_type
  | Ifte    of (values_type * seq_type * seq_type) * location * term_type
  | Pair    of (values_type * values_type) * location * term_type

(** Type of a variable/function declaration *)
and decl_type =
    Vardecl of (string * values_type) * location * term_type
  | Varset  of (string * values_type) * location * term_type
  | Fundecl of (string * string list * seq_type) * location * term_type

(** Type of the sequences (body of a function) *)
and seq_type =
    Seq     of (decl_type * seq_type) * location * term_type
  | Return  of values_type * location * term_type

(** Type of a program *)
and prog_type =
    Globseq of (decl_type * prog_type) * location * term_type
  | Procseq of (procedure_type * prog_type) * location * term_type
  | Empty


(** Unifiable types for the type-checker *)
and static = [
  `Int_tc    |
  `Unit_tc   |
  `String_tc |
  `Bool_tc   |
  `List_tc  of term_type |
  `Array_tc of term_type |
  `Fun_tc   of term_type * term_type |
  `Pair_tc  of term_type * term_type |
  `None
]

and term_type = static ref




(* TODO : Relocate those two in a ScriptEngine module *)

type value_type = [
  `Int_t    |
  `Unit_t   |
  `String_t |
  `Bool_t   |
  `List_t  of value_type |
  `Array_t of value_type |
  `Fun_t   of value_type * value_type |
  `Pair_t  of value_type * value_type 
]

type value = [
  `Int    of int              |
  `Unit                       |
  `String of string           |
  `Bool   of bool             |
  `List   of value list       |
  `Array  of value array      |
  `Fun    of (value -> value) |
  `Pair   of value * value
]
