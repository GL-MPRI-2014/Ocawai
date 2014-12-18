(** Class representing the data hold by the client *)
class client_data : object

  (* Update methods *)
  method pop_update : Types.update

  method push_update : Types.update -> unit

  method top_update : Types.update

  (* Initialization methods *)
  method init_core : Battlefield.t -> 
    ClientPlayer.client_player -> Player.logicPlayer list -> unit

  method init_buildings : Building.t list -> unit

  method init_interface : Camera.camera -> unit

  (* Accessors & functions *)
  method map : Battlefield.t

  method minimap : Minimap.minimap

  method case_info : CaseInfo.case_info

  method camera : Camera.camera

  method players : Player.logicPlayer list

  method neutral_buildings : Building.t list

  method actual_player : ClientPlayer.client_player

  method current_move : Position.t list

  method player_unit_at_position : Position.t ->
    #Player.logicPlayer -> Unit.t option

  method enemy_unit_at_position : Position.t -> bool

  method unit_at_position : Position.t -> Unit.t option

  method player_of : Unit.t -> Player.logicPlayer

  method building_at_position :
    Position.t -> (Building.t option) * (Player.logicPlayer option)

end
