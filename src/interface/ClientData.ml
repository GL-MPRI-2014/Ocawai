class client_data ~(map:Battlefield.t) ~(camera:Camera.camera)
  ~(units:Unit.t list) = object

  method map = map

  method camera = camera

  (* Will be useful later *)
  (* method players : Player.t list *)

  (* Will be deleted later *)
  method units = units

  (* method current_move = current_move *)
  method current_move = camera#cursor#get_move

  method unit_at_position p = 
    let rec aux = function
      |[] -> None
      |t::q when t#position = p -> Some(t)
      |t::q -> aux q
    in aux units

end
