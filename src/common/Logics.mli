(** Some useful functions related to the game's logic *)
type accessibles = Position.t list * (Position.t, Path.t) Hashtbl.t

(** Returns the list of positions that a given unit can see *)
val unit_vision : Unit.t -> Battlefield.t -> Position.t list

(** Returns the list of all positions that a given player can see *)
val player_vision : Player.logicPlayer -> Battlefield.t -> Position.t list

(** For a given unit, returns the list of accessible positions and the 
    hashtable where the keys are the accessible positions and the entry 
    is a shortest path to this position *)
val accessible_positions : Unit.t -> Player.logicPlayer -> Player.logicPlayer list -> Battlefield.t -> Position.t list * (Position.t, Path.t) Hashtbl.t

(** Checks if the requested action is possible.
    If it is, the same action is returned, else the function returns another
    action with the maximum possible path. *)
val try_next_action : Player.logicPlayer list -> Player.logicPlayer -> Unit.t list -> 
  Battlefield.t -> Action.t -> Action.t

val find_unit : Position.t -> Player.logicPlayer -> Unit.t

