(** Type checker *)

(** Raised when to types can't be unified *)
exception Unification_failure

(** Takes a script and type_checks it. *)
val check_prog : ScriptTypes.prog_type -> unit

(** Adds a value to the type checker *)
val expose : ScriptValues.value_type -> string -> unit

(** Removes the type of a previously exposed value type *)
val hide : string -> unit
