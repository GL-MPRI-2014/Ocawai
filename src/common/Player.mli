type log_item =
  | Moved of Unit.t * Action.movement

class logicPlayer :  ?id:Types.id_player -> Unit.t list -> Building.t list  -> object
    method get_id : Types.id_player
    method get_army : Unit.t list
    method add_unit : Unit.t -> unit
    method set_army : Unit.t list -> unit
    method set_buildings : Building.t list -> unit
    method get_buildings : Building.t list
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
