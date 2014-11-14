class client_data ~(map:Battlefield.t) ~(camera:Camera.camera)
  ~(players:Player.t list)= object

  val minimap = new Minimap.minimap 40 
    (fst (Battlefield.size map))
    (snd (Battlefield.size map))

  initializer
    minimap#compute map players

  method map = map

  method camera = camera

  method minimap = minimap

  (* Will be useful later *)
  method players = players

  (* method current_move = current_move *)
  method current_move = camera#cursor#get_move

  method unit_at_position p = 
    let rec aux = function
      |[] -> None
      |t::q when t#position = p -> Some(t)
      |t::q -> aux q
    in 
    let rec aux_player = function 
      |[] -> None
      |t::q -> begin
          match aux t#get_army with 
          |None -> aux_player q
          |Some(s) -> Some(s)
      end
    in aux_player players

  method player_of u = 
    let rec aux = function
      |[] -> false
      |t::q -> t = u || aux q
    in 
    let rec iter_player = function
      |[] -> assert false
      |t::q -> if aux t#get_army then t else iter_player q
    in 
    iter_player players

end
