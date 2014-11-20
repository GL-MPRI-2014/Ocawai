(** Action functions *)

type movement = Position.t list

type action = 
| Attack_unit of (Unit.t * Unit.t)
| Attack_building of (Unit.t * Building.t)
| Wait
| End_turn

type t = movement * action

type logic_player = <
  get_army : Unit.t list;
  get_buildings : Building.t list
>
    
exception Bad_unit
exception Has_played
exception Bad_path

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
	
let try_next_action player_list player has_played bf order =
  let mvt = fst order and action = snd order in
  if action = End_turn then
    order
  else (
    let source = List.hd mvt in
    let u = find_unit source player in (*may raise Bad_unit*)
    if List.mem u has_played then raise Has_played;
    let (real_mvt, is_equal) = 
      try_movement u bf player player_list mvt (*may raise Bad_path*)
    in
    if not is_equal then 
      (real_mvt, Wait) (*if the movement was shortened, unit does nothing*)
    else (*in this case, real_mvt is equal to mvt*)
      match action with
      | Wait -> (mvt, Wait)
      | End_turn -> failwith "try_next_action: this case is not possible"
      | Attack_unit (att, def) 
      | Attack_building (att, def) ->
	if att <> u then (mvt, Wait) (*only the unit that moved can attack*)
	else (
	  let dist = Position.dist (att#position) (def#position) in
	  let range = (att#min_attack_range, att#attack_range) in
	  if fst range > dist || snd range < dist then 
	    (mvt, Wait) (*targeted unit not in range; do nothing*)
	  else if snd range > 1 && List.length mvt > 1 then
	    (mvt, Wait) (*a ranged unit must not move before firing*)
	  else
	    (mvt, action) (*the attack is valid*)
      | _ -> (mvt, Wait)
	(*New actions here*)
  )

let apply_fight src dst = ()
