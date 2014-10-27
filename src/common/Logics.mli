(** Some useful functions related to the game's logic *)

(** Return the list of positions that a given unit can see *)
val unit_vision : Unit.t -> Battlefield.t -> Position.t list

(** Return the list of all positions that a given player can see *)
val player_vision : Player.t -> Battlefield.t -> Position.t list
