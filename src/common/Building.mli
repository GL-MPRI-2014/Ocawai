(** Building interface (draft) *)


type id = int

type unbound_t = <
  name : string;
  product : string list;
  income : int;
  spawn_number_per_player : int;
  spawn_number_neutral : int;
  movement_types : Unit.movement list
>

type t = <
  name : string;
  position : Position.t;
  get_id : id;
  player_id : int option;
  product : string list;
  income : int;
  spawn_number_per_player : int;
  spawn_number_neutral : int;
  set_owner : int -> unit;
  set_neutral : unit;
  movement_types : Unit.movement list
>

val bind : unbound_t -> Position.t -> int option -> t
val bind_extended : unbound_t -> Position.t -> int option -> id -> t

val create_unbound_from_parsed_building : Building_t.t -> unbound_t
val create_parsed_building_from_unbound : unbound_t -> Building_t.t








