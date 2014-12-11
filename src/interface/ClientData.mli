(** Class representing the data hold by the client *)
class client_data : map:Battlefield.t -> camera:Camera.camera ->
  players:(Player.logicPlayer list) ->
  actual_player:(ClientPlayer.client_player) ->
  neutral_buildings:(unit -> Building.t list) -> object

  method map : Battlefield.t

  method minimap : Minimap.minimap

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

end
