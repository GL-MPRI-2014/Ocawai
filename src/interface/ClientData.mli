(** Class representing the data hold by the client *)
class client_data : map:Battlefield.t -> camera:Camera.camera ->
  units:(Unit.t list) -> object

  method map : Battlefield.t

  method camera : Camera.camera

  (* Will be useful later *)
  (* method players : Player.t list *)

  (* Will be deleted later *)
  method units : Unit.t list

  method current_move : Position.t list

  method unit_at_position : Position.t -> Unit.t option

end
