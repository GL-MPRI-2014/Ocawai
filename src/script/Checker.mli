(** Type checker *)

(** Raised when the type checking fails *)
exception Type_checking_failure

(** Takes a script and type_checks it. *)
(** Raises [Type_checking_failure] in case it doesn't type. *)
val type_check : ScriptTypes.prog_type -> unit

(** Adds a value to the type checker *)
val expose : ScriptValues.value_type -> string -> unit

(** Removes the type of a previously exposed value type *)
val hide : string -> unit
