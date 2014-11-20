(** Class representing the data hold by the client *)
class client_data : map:Battlefield.t -> camera:Camera.camera ->
  players:(Player.logicPlayer list) ->
  actual_player:(ClientPlayer.client_player) -> object

  method map : Battlefield.t

  method minimap : Minimap.minimap

  method camera : Camera.camera

  method players : Player.logicPlayer list

  method actual_player : ClientPlayer.client_player 

  method current_move : Position.t list

  method player_unit_at_position : Position.t -> 
    #Player.logicPlayer -> Unit.t option

  method unit_at_position : Position.t -> Unit.t option

  method player_of : Unit.t -> Player.logicPlayer

end
