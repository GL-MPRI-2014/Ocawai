(** Basic types for the script parser *)

(** Type of procedures called by the interpreter *)
type procedure_type =
    Move   of string list * seq_type
  | Attack of string list * seq_type
  | Main of seq_type
  | Init of seq_type

(** Type of the values manipulated by the AIs *)
and values_type =
    Int     of int
  | Unit
  | String  of string
  | Bool    of bool
  | List    of values_type list
  | Array   of values_type array
  | Var     of string
  | App     of string * values_type list
  | Ifte    of values_type * seq_type * seq_type
  | Pair    of values_type * values_type

(** Type of a variable/function declaration *)
and decl_type =
    Vardecl of string * values_type
  | Varset  of string * values_type
  | Fundecl of string * string list * seq_type

(** Type of the sequences (body of a function) *)
and seq_type =
    Seq     of decl_type * seq_type
  | Return  of values_type

(** Type of a program *)
and prog_type =
    Globseq of decl_type * prog_type
  | Procseq of procedure_type * prog_type
  | Empty


(** Static types *)

(* type typed_ast = prog_type * static *)

type static = [
  `Int |
  `Unit |
  `String |
  `Bool |
  `List of static |
  `Array of static |
  `Fun of static * static
]
