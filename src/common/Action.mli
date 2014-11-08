(** Action Interface *)

(** Movement type for actions.
    The head of the list is the source of the movement, the last element is
    the destination. *)
type movement = Position.t list

type action = 
| Attack_unit of (Unit.t * Unit.t)
| Attack_building of (Unit.t * Building.t)
| Wait
| End_turn

type t = movement * action

type logic_player

(** Exception raised if the unit moving does not exist or does not belong
    to the player *)
exception Bad_unit

(** Exception raised if the unit moving already moved earlier *)
exception Has_played

(** Exception raised if the movement is not possible (i.e. too long or
    passing through impassable terrain) *)
exception Bad_path

(** Checks if the requested action is possible.
    If it is, the same action is returned, else the function returns another
    action with the maximum possible path. *)
val try_next_action : logic_player list -> logic_player -> Unit.t list -> 
  Battlefield.t -> t -> t

