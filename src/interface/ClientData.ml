class client_data
  ~(map:Battlefield.t)
  ~(camera:Camera.camera)
  ~(players:Player.logicPlayer list)
  ~(actual_player:ClientPlayer.client_player)
  ~(neutral_buildings:(unit -> Building.t list)) = object(self)

  val minimap = new Minimap.minimap 30
    (fst (Battlefield.size map))
    (snd (Battlefield.size map))

  initializer
    minimap#compute map players

  method map = map

  method camera = camera

  method minimap = minimap

  method players = players

  method neutral_buildings = neutral_buildings ()

  method actual_player = actual_player

  method current_move = camera#cursor#get_move

  method player_unit_at_position :
    'a. Position.t -> (#Player.logicPlayer as 'a) -> Unit.t option
    = fun pos player ->
      let rec unit_aux = function
        |[] -> None
        |t::q when t#position = pos -> Some(t)
        |t::q -> unit_aux q
      in unit_aux player#get_army

  method unit_at_position p =
    let rec aux_player = function
      |[] -> None
      |t::q -> begin
          match self#player_unit_at_position p t with
          |None -> aux_player q
          |Some(s) -> Some(s)
      end
    in aux_player players

  method enemy_unit_at_position p =
    let u  = self#unit_at_position p in
    let u' = self#player_unit_at_position p actual_player in
    match u with
    |Some(_) when u' = None -> true
    | _ -> false

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
