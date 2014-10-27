(** Some useful functions related to the game's logic *)

(** Returns true iff the given position is within the battlefield *)
val in_battlefield : Battlefield.t -> Position.t -> bool

(** Returns the list of positions that a given unit can see *)
val unit_vision : Unit.t -> Battlefield.t -> Position.t list

(** Returns the list of all positions that a given player can see *)
val player_vision : Player.t -> Battlefield.t -> Position.t list

(** For a given unit, returns the hashtable where the keys are the
    accessible positions and the entry is a shortest path to this position *)
val accessible_positions : Unit.t -> Player.t -> Player.t list -> Battlefield.t -> (Position.t, Pathfinder.t) Hashtbl.t
