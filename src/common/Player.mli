
type t = <
  get_army : Unit.t list;
  set_army : Unit.t list -> unit;
  add_unit : Unit.t -> unit;
  get_buildings : Building.t list;
  add_building : Building.t -> unit;
  get_next_action : Action.t
>

val create_player : unit -> t
