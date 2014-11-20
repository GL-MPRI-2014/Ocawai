(* We might want to change it later, leave it hidden *)
(* We want to make sure there is no loop and discontinuity in it *)
exception Path_exception of string

type t = Position.t list

let empty = []

let init pos = [pos]

(* The empty case is arbitrary and might need to be changed *)
let rec back_to pos = function
  | p :: r when p = pos -> p :: r
  | p :: r -> back_to pos r
  | [] -> [pos]

let reach path pos =
  if List.mem pos path then back_to pos path
  else if Position.dist pos (List.hd path) = 1 then pos :: path
  else raise (Path_exception "discontinuous path")

let get_move = List.rev

let start_position path  =
  match path with
  | [] -> raise (Path_exception "Empty path")
  | _ -> List.hd (List.rev path)

let final_position path =
  match path with  
  | [] -> raise (Path_exception "Empty path")
  | _ -> List.hd path



let cat p1 p2 = p2 @ p1

let rec cost mvt_type m path = match path with
  | [] -> 0 (*arbitrary value*)
  | [pos] -> 0
  | pos :: t ->
    let tile = Battlefield.get_tile m pos in
    assert (Tile.traversable_m tile mvt_type);
    (Tile.movement_cost tile mvt_type) + (cost mvt_type m t)

let access a p = let (a1,a2) = Position.topair p in a.(a1).(a2)
let change a p v = let (a1,a2) = Position.topair p in a.(a1).(a2) <- v
  
(* construit le chemin jusqu'à pos en remontant  prev *)
let rec rev_path prev = function
| None -> []
| Some prev_pos -> prev_pos::(rev_path prev (access prev prev_pos))

let walk_on_canvas m pos move_type pos2 w h dist prev f =
  (* renvoie Some "cout de la tuile en Position.create(a,b)" ou None si pas traversable *)
  let cost a = 
    let ti = (Battlefield.get_tile m a) in 
    if (Tile.traversable_m ti move_type) then Some (Tile.movement_cost ti move_type) else None 
  in
  (* sommets non parcourus *)
  let li = PosPrioQueue.empty w h in 
  let p_none = PosPrioQueue.p_none in
  (* initialisations *)
  change dist pos (Some 0);
  PosPrioQueue.push li 0 pos;
  Battlefield.tile_iteri (fun p t -> if (Tile.traversable_m t move_type) && p <> pos then PosPrioQueue.push li p_none p) m;
  (* boucle principale *)
  let min_cost = ref (-1) in
  while not (PosPrioQueue.is_empty li) do
    (* u : min (dist a + Position.dist a pos2) pour tout a non parcouru *)
    let top = PosPrioQueue.pop li in
    let u = snd top in
    if u = pos2 then min_cost := fst top else 
    if !min_cost >= 0 && fst top > !min_cost then PosPrioQueue.set_empty li else
    match access dist u with
    | None -> PosPrioQueue.set_empty li
    | Some du -> (* pour tout voisin v de u atteignable d'une autre façon, on teste si c'est plus court d'aller en v par u *)
        let shorter_path v cv = 
          let alternate_cost = du + cv in
          if match access dist v with | None -> true | Some dv -> alternate_cost < dv then
          begin
            change dist v (Some alternate_cost);
            change prev v (Some u);
            PosPrioQueue.decrease_priority li (alternate_cost + f v pos2) v;
          end
        in
        List.iter (fun v -> let open Utils in (cost v) >? (shorter_path v)) 
                  ( List.filter (Battlefield.in_range m) (Utils.shuffle [Position.up u; Position.down u; Position.left u; Position.right u]) );
  done

let dijkstra m pos move_type =
  let (w,h) = Battlefield.size m in
  (* table des distances à pos *)
  let dist = Array.make_matrix w h None in
  (* prev.(x).(y) est la position de la tuile precedant Position.create(x,y) sur le chemin de pos a Position.create(x,y) *)
  let prev = Array.make_matrix w h None in
  walk_on_canvas m pos move_type (Position.create(-1,-1)) w h dist prev (fun _ _ -> 0);
  (* construit le chemin jusqu'à pos en remontant  prev *)
  (fun pos2 ->  
      match access dist pos2 with
      | None -> None
      | Some dab -> Some (dab , rev_path prev (Some pos2) ) 
  )

let a_star m pos move_type pos2 =  
  let (w,h) = Battlefield.size m in
  (* table des distances à pos *)
  let dist = Array.make_matrix w h None in
  (* prev.(x).(y) est la position de la tuile precedant Position.create(x,y) sur le chemin de pos a Position.create(x,y) *)
  let prev = Array.make_matrix w h None in
  walk_on_canvas m pos move_type pos2 w h dist prev Position.dist;
  (* construit le chemin jusqu'à pos en remontant  prev *)
  match access dist pos2 with
  | None -> None
  | Some dab -> Some (dab , rev_path prev (Some pos2) )

let rec print_path = function
  |[] -> print_endline ""
  |[t] -> 
      let t' = Position.topair t in 
      Printf.printf "(%i,%i)" (fst t') (snd t')
  |t::q -> 
      let t' = Position.topair t in 
      Printf.printf "(%i,%i) - " (fst t') (snd t'); print_path q
