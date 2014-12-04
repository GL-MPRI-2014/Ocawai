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

(** Same as accessible_positions, but without positions where an allied unit
    is already stationned. *)
val available_positions : Unit.t -> Player.logicPlayer -> 
  Player.logicPlayer list -> Battlefield.t -> accessibles

(** Checks if the requested action is possible.
    If it is, the same action is returned, else the function returns another
    action with the maximum possible path. *)
val try_next_action : Player.logicPlayer list -> Player.logicPlayer -> 
  Battlefield.t -> Action.t -> Action.t

(** Returns the unit of the given player at the given position.
    Raises Bad_unit if it could not be found. *)
val find_unit : Position.t -> Player.logicPlayer -> Unit.t

(** Find the player with given id from the list of players *)
val find_player : int -> Player.logicPlayer list -> Player.logicPlayer

(** Computes and apply the damage of an attack. *)
val apply_attack : Unit.t -> Unit.t -> unit

(** Computes owner changes for buildings at the start of a turn *)
val capture_buildings : Player.logicPlayer list -> Player.logicPlayer ->
  Building.t list -> (Building.t * (Player.logicPlayer option)) list

(** Returns the list of the ennemy units in range *)
val units_inrange : Position.t -> int -> Player.logicPlayer -> 
  Player.logicPlayer list -> Unit.t list
  
