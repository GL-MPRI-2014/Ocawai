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
  * This should return the next unit to play *)
val main_script : script -> Unit.t 

(** [move_script s u] calls the move methods of the script [s]
  * @return a move for the unit [u] *)
val move_script : script -> Unit.t -> Action.movement

(** [attack_script s u] calls the attack methods of the script [s]
  * @return a unit attacked by the unit [u] *)
val attack_script : script -> Unit.t -> Unit.t
