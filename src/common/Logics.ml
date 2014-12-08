open Action

type accessibles = Position.t list * (Position.t, Path.t) Hashtbl.t

let unit_vision (unit : Unit.t) (bf : Battlefield.t) : Position.t list =
  let l = Position.filled_circle (unit#position) (unit#vision_range) in
  List.filter (Battlefield.in_range bf) l


let rec remove_double l = match l with
  | [] -> []
  | h::t -> h :: (remove_double (List.filter (fun e -> e <> h) t))


let player_vision (player : Player.logicPlayer) (bf : Battlefield.t) : Position.t list =
  let l = List.fold_left
    (fun list unit -> list @ (unit_vision unit bf))
    [] player#get_army
  in
  remove_double l


let rec dfs bf player mvt_point mvt_type visible_pos unit_pos h visited l pos path only_available =
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
    let newpath = Path.reach path pos in
    let allied_unit = ref false in
    (* now we check if we can actually go to this new position *)
    if cost <= mvt_point
      && (not (Hashtbl.mem visited pos)
	  || (Path.cost mvt_type bf (Hashtbl.find visited pos) >
	      Path.cost mvt_type bf newpath)
      )
      && (not (List.mem pos visible_pos)
	  || not (Hashtbl.mem unit_pos pos)
	  || (allied_unit := (snd (Hashtbl.find unit_pos pos) = player);
	      !allied_unit)
      )
    then (
      (* if only_available is set to true, add positions only if there is also
	 no allied unit on it *)
      if not (Hashtbl.mem h pos) && not (only_available && (!allied_unit)) then
	l := pos::(!l);
      if not (only_available && (!allied_unit)) then 
	Hashtbl.replace h pos newpath;
      Hashtbl.replace visited pos newpath;
      let mvt_point = mvt_point - cost in
      dfs bf player mvt_point mvt_type visible_pos unit_pos h visited l pos 
	newpath only_available
    )
  in
  List.iter visit neighbour


let accessible_positions_aux unit player player_list bf only_available =
  if not (List.mem unit player#get_army) then
    failwith "accessible_positions: wrong player";
  let visible_pos = player_vision player bf in
  let unit_pos = Hashtbl.create 101 in
  List.iter
    (fun player -> List.iter
      (fun unit -> Hashtbl.add unit_pos unit#position (unit,player))
      player#get_army
    )
    player_list;
  let h = Hashtbl.create 101 in
  let visited = Hashtbl.create 101 in
  let l = ref [unit#position] in
  let path_init = Path.init unit#position in
  Hashtbl.add h unit#position path_init;
  Hashtbl.add visited unit#position path_init;
  dfs bf player unit#move_range unit#movement_type visible_pos unit_pos h 
    visited l unit#position path_init only_available;
  (!l,h)

let accessible_positions unit player player_list bf =
  accessible_positions_aux unit player player_list bf false

let available_positions unit player player_list bf =
  accessible_positions_aux unit player player_list bf true








let find_unit pos player : Unit.t =
  let l = player#get_army in
  let rec aux l = match l with
    | [] -> raise Bad_unit
    | u :: t -> if u#position = pos then u else aux t
  in
  aux l

(* Returns (b1,b2) two booleans where
   - b1 iff there is a unit on position pos
   - if b1 then (b2 iff this unit belongs to player) *)
let unit_of_position pos player player_list =
  let rec check_army l = match l with
    | [] -> false
    | u :: t -> u#position = pos || check_army t
  in
  let rec check_players l = match l with
    | [] -> (false,true)
    | p :: t ->
      if check_army p#get_army then (true, p = player)
      else check_players t
  in
  check_players player_list

(* Returns the subpath of path which stops when reaching pos *)
let rec subpath path pos = match path with
  | [] -> []
  | p :: t -> 
    if p = pos then [p]
    else p :: (subpath t pos)

(* Returns (mvt, b) where
   - mvt is the actual movement done
   - b iff mvt is equal to the wanted movement *)
let try_movement unit bf player player_list mvt =
  let dest = List.hd (List.rev mvt) in
  if dest <> unit#position && 
     unit_of_position dest player player_list = (true,true)
  then raise Bad_path; (*allied unit at the end of the movement*)
  let mvt_pt = unit#move_range in
  let last_viable_pos = ref (List.hd mvt) in
  let rec aux mvt_pt mvt = match mvt with
    | [] -> failwith "Action.try_movement: bad movement"
    | [dst] -> true
    | src :: dst :: t ->
      let tile = Battlefield.get_tile bf dst in
      if not (Tile.traversable tile unit) then raise Bad_path;
      let cost = Tile.tile_cost tile unit in
      if mvt_pt - cost < 0 then raise Bad_path;
      let (b1,b2) = unit_of_position dst player player_list in
      if b1 then
	b2 && (t <> []) && aux (mvt_pt - cost) (dst::t)
      else (
	last_viable_pos := dst;
	aux (mvt_pt - cost) (dst::t)
      )
  in
  if aux mvt_pt mvt then (mvt, true)
  else (subpath mvt (!last_viable_pos), false)
	
let try_next_action player_list player bf order =
  let mvt = fst order and action = snd order in
  if action = End_turn then
    order
  else (
    let source = List.hd mvt in
    let u = find_unit source player in (*may raise Bad_unit*)
    if u#has_played then raise Has_played;
    let (real_mvt, is_equal) = 
      try_movement u bf player player_list mvt (*may raise Bad_path*)
    in
    if not is_equal then 
      (real_mvt, Wait) (*if the movement was shortened, unit does nothing*)
    else (*in this case, real_mvt is equal to mvt*)
      match action with
      | Wait -> (mvt, Wait)
      | End_turn -> failwith "try_next_action: this case is not possible"
      | Attack_unit (att, def) ->
        let dest = List.nth mvt (List.length mvt - 1) in
	if att <> u then raise Bad_attack (*only the unit who moved can attack*)
	else (
	  let dist = Position.dist dest (def#position) in
	  let range = (att#min_attack_range, att#attack_range) in
	  if fst range > dist || snd range < dist then 
	    raise Bad_attack (*targeted unit not in range*)
          (*commented for testing purposes *) 
	  (*else if snd range > 1 && List.length mvt > 1 then
	    raise Bad_attack (*a ranged unit must not move before firing*)*)
	  else
	    (mvt, action) (*the attack is valid*)
	)
      | _ -> (mvt, Wait)
	(*New actions here*)
  )

let apply_attack att def =
  let lambda = 90 and mu = 10 in
  let percentage = lambda * att#hp + mu * att#life_max in
  let div = 100 * att#life_max in
  let damage = att#attack def#armor (percentage * 100 / div) in
  (* coeff = 0.9 * (current hp/max hp) + 0.1 *)
  def#take_damage damage

let rec find_player id player_list = match player_list with
  | [] -> failwith "find_player: not found"
  | p :: t -> 
    if p#get_id = id then p 
    else find_player id t

let capture_buildings player_list player building_list =
  let unit_list = player#get_army in
  let p_id = player#get_id in
  let changed = ref [] in
  let aux u =
    match u#movement_type with
    | Unit.Walk | Unit.Roll | Unit.Tread 
    | Unit.Amphibious_Walk | Unit.Amphibious_Roll | Unit.Amphibious_Tread -> (
      let pos = u#position in
      let rec find_building = function
	| [] -> ()
	| b :: t -> 
	  if b#position = pos then (
	    match b#player_id with
	    | None -> 
	      b#set_owner p_id;
	      changed := (b, None) :: (!changed)
	    | Some id when id = p_id ->
	      ()
	    | Some id ->
	      b#set_neutral;
	      changed := (b, Some (find_player id player_list)) :: (!changed)
	  )
	  else find_building t
      in
      find_building building_list
    )
    | _ -> ()
  in
  List.iter aux unit_list;
  (!changed)

let units_inrange pos range player pl = 
  let ennemy_list = List.filter (fun p -> p <> player) pl in
  List.map (fun p ->
    List.filter (fun u ->
      Position.dist pos u#position <= range
    ) p#get_army
  ) ennemy_list
  |> List.flatten

