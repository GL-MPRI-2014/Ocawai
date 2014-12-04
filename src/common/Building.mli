(** Building interface (draft) *)


type id = int

type unbound_t = <
  name : string;
  product : Unit.t list;
  get_income : int
>

type t = <
  name : string;
  position : Position.t;
  get_id : id;
  player_id : int option;
  producible : Unit.t list;
  get_income : int;
  set_owner : int -> unit;
  set_neutral : unit
>

val bind : unbound_t -> Position.t -> int option -> t
val bind_extended : unbound_t -> Position.t -> int option -> id -> t

val create_unbound_from_parsed_building : Building_t.t -> unbound_t
val create_parsed_unit_from_unbound : unbound_t -> Building_t.t








