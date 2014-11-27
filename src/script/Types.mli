(** Basic types for the script parser *)

(** Static types *)
type static = [
  `Int_t    |
  `Unit_t   |
  `String_t |
  `Bool_t   |
  `List_t  of static |
  `Array_t of static |
  `Fun_t   of static * static |
  `Pair_t  of static * static
]

(** Type of procedures called by the interpreter *)
type procedure_type =
    Move   of (string list * seq_type) * term_type
  | Attack of (string list * seq_type) * term_type
  | Main of seq_type * term_type
  | Init of seq_type * term_type

(** Type of the values manipulated by the AIs *)
and values_type =
    Int     of int * term_type
  | Unit    of term_type
  | String  of string * term_type
  | Bool    of bool * term_type
  | List    of values_type list * term_type
  | Array   of values_type array * term_type
  | Var     of string * term_type
  | App     of (string * values_type list) * term_type
  | Ifte    of (values_type * seq_type * seq_type) * term_type
  | Pair    of (values_type * values_type) * term_type

(** Type of a variable/function declaration *)
and decl_type =
    Vardecl of (string * values_type) * term_type
  | Varset  of (string * values_type) * term_type
  | Fundecl of (string * string list * seq_type) * term_type

(** Type of the sequences (body of a function) *)
and seq_type =
    Seq     of (decl_type * seq_type) * term_type
  | Return  of values_type * term_type

(** Type of a program *)
and prog_type =
    Globseq of (decl_type * prog_type) * term_type
  | Procseq of( procedure_type * prog_type) * term_type
  | Empty

and term_type = [static | `None] ref

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
