(** Some useful functions related to the game's logic *)
type accessibles = Position.t list * (Position.t, Path.t) Hashtbl.t

(** @return the list of positions that a given unit can see *)
val unit_vision : Unit.t -> Battlefield.t -> Position.t list

(** @return the list of all positions that a given player can see *)
val player_vision : Player.logicPlayer -> Battlefield.t -> Position.t list

(** [is_unit_on pos player_list] returns true iff there is an unit on 
    position [pos]*)
val is_unit_on : Position.t -> Player.logicPlayer list -> bool

(** For a given unit, returns the list of accessible positions and the
    hashtable where the keys are the accessible positions and the entry
    is a shortest path to this position *)
val accessible_positions : Unit.t -> Player.logicPlayer -> Player.logicPlayer list -> Battlefield.t -> accessibles

(** Same as accessible_positions, but without positions where an allied unit
    is already stationned. *)
val available_positions : Unit.t -> Player.logicPlayer ->
  Player.logicPlayer list -> Battlefield.t -> accessibles

(** Checks if the requested action is possible.
    If it is, the same action is returned, else the function returns another
    action with the maximum possible path. *)
val try_next_action : Player.logicPlayer list -> Player.logicPlayer ->
  Battlefield.t -> Action.t -> Action.t

(** [find_unit pos player] returns the unit of [player] at position [pos].
    Raises Bad_unit if it could not be found. *)
val find_unit : Position.t -> Player.logicPlayer -> Unit.t

(** [find_player id player_list] returns the player with id [id] *)
val find_player : int -> Player.logicPlayer list -> Player.logicPlayer

(** [apply_attack att def] computes and apply the damage of an attack of [att]
    against [def]. *)
val apply_attack : Unit.t -> Unit.t -> unit

(** Computes owner changes for buildings at the start of a turn *)
val capture_buildings : Player.logicPlayer list -> Player.logicPlayer ->
  Building.t list -> (Building.t * (Player.logicPlayer option)) list

(** @return the minimum and maximum damage for an attack *)
val damage_interval : Unit.t -> Unit.t -> int * int

(** Returns the list of the ennemy units in range *)
val units_inrange : Position.t -> (int*int) -> Player.logicPlayer -> 
  Player.logicPlayer list -> Unit.t list
