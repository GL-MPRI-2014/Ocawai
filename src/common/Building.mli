(** Building interface (draft) *)

type building_id = int

(** Type for buildings without a position nor id, player_id, life...
    Intended to represent a building before its instanciation via bind *)
type unbound_t = <
  name : string;
  product : string list;
  income : int;
  vision_range : int;
  spawn_number_per_player : int;
  spawn_number_neutral : int;
  movement_types : Unit.movement list
>

(** Building type if bound*)
type t = <
  name : string;
  position : Position.t;
  get_id : building_id;
  player_id : int option;
  product : string list;
  income : int;
  vision_range : int;
  spawn_number_per_player : int;
  spawn_number_neutral : int;
  set_owner : int -> unit;
  set_neutral : unit;
  movement_types : Unit.movement list
>

(** Bound a building to a position and myabe a player*)
val bind : unbound_t -> Position.t -> int option -> t
(** Bound a building to a position and maybe a player using if the building id is known*)
val bind_extended : unbound_t -> Position.t -> int option -> building_id -> t

(** Create a building from a parsed record*)
val create_unbound_from_parsed_building : Building_t.t -> unbound_t

(** Create a parsed record from a building
    @see Config.mli *)
val create_parsed_building_from_unbound : unbound_t -> Building_t.t








