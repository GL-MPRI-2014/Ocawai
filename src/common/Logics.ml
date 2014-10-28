let unit_vision (unit : Unit.t) (bf : Battlefield.t) : Position.t list =
  let l = Position.filled_circle (unit#position) (unit#vision_range) in
  List.filter (Battlefield.in_range bf) l


let rec remove_double l = match l with
  | [] -> []
  | h::t -> h :: (remove_double (List.filter (fun e -> e <> h) t))


let player_vision (player : Player.t) (bf : Battlefield.t) : Position.t list =
  let l = List.fold_left 
    (fun list unit -> list @ (unit_vision unit bf))
    [] player#get_army
  in
  remove_double l


let rec dfs bf player mvt_point mvt_type visible_pos unit_pos h pos path =
  if mvt_point < 0 then failwith "dfs: mvt_point < 0";
  let neighbour_unsafe = 
    [Position.left pos; Position.up pos; Position.right pos; Position.down pos]
  in
  let neighbour = List.filter (Battlefield.in_range bf) neighbour_unsafe in
  let visit pos =
    let tile = Battlefield.get_tile bf pos in
    let cost = 
      if Tile.traversable_m tile mvt_type then
	Tile.movement_cost tile mvt_type
      else mvt_point + 1
    in
    let newpath = Pathfinder.reach path pos in
    (* now we check if we can actually go to this new position *)
    if cost <= mvt_point 
      && (not (Hashtbl.mem h pos)
	  || (Pathfinder.cost mvt_type bf (Hashtbl.find h pos) >
	      Pathfinder.cost mvt_type bf newpath)
      )
      && (not (List.mem pos visible_pos)
	  || not (Hashtbl.mem unit_pos pos)
	  || snd (Hashtbl.find unit_pos pos) = player
      )   
    then (
      Hashtbl.add h pos newpath;
      let mvt_point = mvt_point - cost in
      dfs bf player mvt_point mvt_type visible_pos unit_pos h pos newpath
    )
  in
  List.iter visit neighbour


let accessible_positions unit player player_list bf =
  if not (List.mem unit player#get_army) then 
    failwith "accessible_positions: wrong player";
  let visible_pos = player_vision player bf in
  let unit_pos = Hashtbl.create 50 in
  List.iter 
    (fun player -> List.iter 
      (fun unit -> Hashtbl.add unit_pos unit#position (unit,player))
      player#get_army
    )
    player_list;
  let h = Hashtbl.create 50 in
  let path_init = Pathfinder.init unit#position in
  Hashtbl.add h unit#position path_init;
  dfs bf player unit#move_range unit#movement_type visible_pos unit_pos h 
    unit#position path_init;
  h
  
