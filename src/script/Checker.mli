(** Type checker *)

(** Takes a script and type_checks it. *)
val type_check : ScriptTypes.prog_type -> unit

(** Adds a value to the type checker *)
val expose : ScriptValues.value_type -> string -> unit

(** Removes the type of a previously exposed value type *)
val hide : string -> unit
