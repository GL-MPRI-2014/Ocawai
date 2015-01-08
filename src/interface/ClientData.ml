let get_option o =
  match o with
  |Some(t) -> t
  |None -> failwith "Bad client data initialization"

class client_data = object(self)

  val mutable minimap = None
    (*new Minimap.minimap 30
    (fst (Battlefield.size map))
    (snd (Battlefield.size map))*)

  val case_info = new CaseInfo.case_info

  val updates : Types.update Queue.t = Queue.create ()

  val mutable players : Player.logicPlayer list = []

  val mutable actual_player : ClientPlayer.client_player option = None

  val mutable map : Battlefield.t option = None

  val mutable camera : Camera.camera option = None

  val mutable neutral_buildings : Building.t list = []

  val mutable mutex = Mutex.create ()

  method init_core m self_player player_list =
    let minim = new Minimap.minimap 30
      (fst (Battlefield.size m))
      (snd (Battlefield.size m))
    in
    minim#compute m player_list;
    minimap <- Some(minim);
    players <- player_list;
    actual_player <- Some (self_player);
    map <- Some(m)

  method mutex = mutex

  method init_buildings neutral =
    neutral_buildings <- neutral

  method init_interface cam =
    camera <- Some cam

  method pop_update =
    try Some (Queue.pop updates)
    with Queue.Empty -> None

  method push_update u = Queue.push u updates

  method top_update =
    try Some (Queue.top updates)
    with Queue.Empty -> None

  method update_iter f = Queue.iter f updates

  method map = get_option map

  method camera = get_option camera

  method minimap = get_option minimap

  method case_info = case_info

  method players = players

  method neutral_buildings = neutral_buildings

  method toggle_neutral_building b =
    if List.exists (fun bd -> bd#get_id = b#get_id) neutral_buildings then
      neutral_buildings <-
        List.filter (fun bd -> bd#get_id <> b#get_id) neutral_buildings
    else
      neutral_buildings <- b :: neutral_buildings

  method actual_player =
    match actual_player with
    |None -> failwith "Bad client data initialization"
    |Some(p) -> p

  method current_move = (get_option camera)#cursor#get_move

  method player_unit_at_position :
    'a. Position.t -> (#Player.logicPlayer as 'a) -> Unit.t option
    = fun pos player ->
      let rec unit_aux = function
        |[] -> None
        |t::q when t#position = pos -> Some(t)
        |t::q -> unit_aux q
      in unit_aux player#get_army

  method player_visible_unit_at_position pos player =
    let rec unit_aux = function
      | [] -> None
      | e :: r when e#position = pos -> Some e
      | e :: r -> unit_aux r
    in unit_aux (self#actual_player#get_visible_army_for player)

  method unit_at_position p =
    let rec aux_player = function
      |[] -> None
      |t::q -> begin
          match self#player_unit_at_position p t with
          |None -> aux_player q
          |Some(s) -> Some(s)
      end
    in aux_player players

  method visible_unit_at_position p =
    let rec aux_player = function
      |[] -> None
      |t::q -> begin
          match self#player_visible_unit_at_position p t with
          |None -> aux_player q
          |Some(s) -> Some(s)
        end
    in aux_player players

  method enemy_unit_at_position p =
    let u  = self#unit_at_position p in
    let u' = self#player_unit_at_position p self#actual_player in
    match u with
    |Some(_) when u' = None -> true
    | _ -> false

  method player_of (u:Unit.t) =
    let rec iter = function
    | []     -> failwith "player not found"
    | p :: r -> if p#get_id = u#player_id then p else iter r
    in iter players

  method private listed_building_at_position pos blist =
    let rec aux = function
      | [] -> None
      | e :: r when e#position = pos -> Some e
      | _ :: r -> aux r
    in aux blist

  method building_at_position p =
    let rec aux_player = function
      | [] -> None, None
      | e :: r ->
          begin
            match self#listed_building_at_position p e#get_buildings with
            | None -> aux_player r
            | Some b -> (Some b, Some e)
          end
    in
    match aux_player players with
      | None, _ ->
          (self#listed_building_at_position p self#neutral_buildings, None)
      | x -> x

end
