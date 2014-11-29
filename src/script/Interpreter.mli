(** Script interpreter *)

type script

exception Unbound_variable of string

exception Unbound_function of string

exception Entry_point_missing of string

(** Creates a script from an AST.
  * The global part of the script will be interpreted. *)
val new_script : ScriptTypes.prog_type -> script

(** Calls the init method of a script *)
val init_script : script -> unit

(** Calls the main method of a script. 
  * This should return the position of the next unit to play *)
val main_script : script -> Position.t 

(** [action_script s u] calls the move and attack methods of the script [s]
  * @return a move for the unit [u] and a position to attack *)
val action_script : script -> Unit.t -> (Action.movement * Position.t)
