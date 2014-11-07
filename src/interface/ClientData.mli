(** Class representing the data hold by the client *)
class client_data : map:Battlefield.t -> camera:Camera.camera ->
  players:(Player.t list) -> object

  method map : Battlefield.t

  method camera : Camera.camera

  method players : Player.t list

  method current_move : Position.t list

  method unit_at_position : Position.t -> Unit.t option

  method player_of : Unit.t -> Player.t

end
