class logicPlayer : Unit.t list -> Building.t list  -> object
    method get_id : int
    method get_army : Unit.t list
    method add_unit : Unit.t -> unit
    method set_army : Unit.t list -> unit
    method set_buildings : Building.t list -> unit
    method get_buildings : Building.t list
    method add_building : Building.t -> unit

    method delete_unit : Unit.t -> unit
    method move_unit : Unit.t -> Action.movement -> unit
    method delete_building : Building.t -> unit

  end

class virtual player :  Unit.t list -> Building.t list  -> object
    inherit logicPlayer
    method virtual get_next_action :  Action.t
  end

val create_player : unit -> player
val create_dummy_player : Action.t list ->  player
