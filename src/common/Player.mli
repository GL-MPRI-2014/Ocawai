
type t = <
  get_army : Unit.t list;
  add_unit : Unit.t -> unit;
  get_buildings : Building.t list;
  add_building : Building.t -> unit;
  get_next_action : Action.t
>

