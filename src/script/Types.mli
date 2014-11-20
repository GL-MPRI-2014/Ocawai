(** Basic types for the script parser *)

(** Type of procedures called by the interpreter *)
type procedure_type = 
    Move   of string list
  | Attack of string list
  | Main
  | Init

(** Type of the values manipulated by the AIs *)
and values_type = 
    Int     of int 
  | Unit    of unit
  | String  of string
  | Bool    of bool
  | List    of values_type list
  | Array   of values_type array
  | Var     of string
  | App     of string * values_type list
  | Ifte    of values_type * seq_type * seq_type

(** Type of a variable/function declaration *)
and decl_type = 
    Vardecl of string * values_type
  | Fundecl of string * string list * seq_type

(** Type of the sequences *)
and seq_type = 
    Seq     of decl_type * seq_type
  | Return  of values_type

(** Type of a program *)
and prog_type = 
    Globseq of decl_type * prog_type
  | Procseq of procedure_type * prog_type 
  | Empty
    

