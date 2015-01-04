(** Action Interface *)

(** Movement type for actions.
    The head of the list is the source of the movement, the last element is
    the destination. *)
type movement = Position.t list

type action =
| Attack_unit of (Unit.id * Unit.id)
| Create_unit of (Building.id * Unit.unbound_t)
| Wait
| End_turn

type t = movement * action

val from_string : string -> t
val to_string : t -> string
val mov_from_string : string -> movement
val mov_to_string : movement -> string


(** Exception raised if the unit moving does not exist or does not belong
    to the player *)
exception Bad_unit

(** Exception raised if the unit moving already moved earlier *)
exception Has_played

(** Exception raised if the movement is not possible (i.e. too long or
    passing through impassable terrain) *)
exception Bad_path

(** Exception raised if the attack is illegal, i.e. wrong unit attacking,
    attacked unit not in range, or ranged attack just after moving. *)
exception Bad_attack

(** Exception raised if a Create_unit order fails. *)
exception Bad_create
