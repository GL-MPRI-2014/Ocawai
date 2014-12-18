(** Module for context-dependant functions *)

type environment = (string * ScriptValues.value) list

(** Returns a new environment with the new functions *)
val get_functions : environment -> environment
