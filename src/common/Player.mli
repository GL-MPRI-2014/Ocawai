type log_item =
  | Moved of Unit.t * Action.movement

class logicPlayer :  ?id:Types.id_player -> Unit.t list -> Building.t list  -> object
    method get_id : Types.id_player
    method get_army : Unit.t list

    (** @return the part of the army of self visible by the player in parameter *)
    method get_visible_army_for : logicPlayer -> Unit.t list
    method get_fog : Fog.t
    method add_unit : Unit.t -> unit
    method set_army : Unit.t list -> unit
    method set_buildings : Building.t list -> unit
    method get_buildings : Building.t list
    method get_base : Building.t option
    method set_base : Building.t -> unit
    method add_building : Building.t -> unit
    (* TODO *)
    method set_unit_hp : Unit.id -> int -> unit

    (** @return the list of actions taken by this player *)
    method get_log : (int * log_item) list

    method get_unit_by_id : Unit.id -> Unit.t
    method get_building_by_id : Building.id -> Building.t

    method delete_unit : Unit.id -> unit
    method move_unit : Unit.id -> Action.movement -> unit
    method delete_building : Building.id -> unit
    
    method get_value_resource : int
    method use_resource : int -> bool
    method harvest_buildings_income : unit

    method init : Battlefield.t -> logicPlayer list -> unit
  end

class virtual player : ?id:Types.id_player ->  Unit.t list -> Building.t list  -> object
    inherit logicPlayer
    method virtual get_next_action :  Action.t
    method virtual set_logicPlayerList : (logicPlayer list) -> unit
    method virtual get_logicPlayerList : logicPlayer list
    method update : Types.update -> unit
  end

val create_player : unit -> player
val create_dummy_player : Action.t list ->  player
