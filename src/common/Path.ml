(* We might want to change it later, leave it hidden *)
(* We want to make sure there is no loop and discontinuity in it *)
type t = Position.t list

let empty = []

let init pos = [pos]

(* The empty case is arbitrary and might need to be changed *)
let rec back_to pos = function
  | p :: r when p = pos -> p :: r
  | p :: r -> back_to pos r
  | [] -> [pos]

(* We will change it to handle the case when its not continous *)
let reach path pos =
  if List.mem pos path then back_to pos path
  else pos :: path

let get_move = List.rev

let cat p1 p2 = p2 @ p1

let rec cost mvt_type m path = match path with
  | [] -> 0 (*arbitrary value*)
  | [pos] -> 0
  | pos :: t ->
    let tile = Battlefield.get_tile m pos in
    assert (Tile.traversable_m tile mvt_type);
    (Tile.movement_cost tile mvt_type) + (cost mvt_type m t)
     
let dijkstra m pos move_type =

  (* renvoie Some "cout de la tuile en Position.create(a,b)" ou None si pas traversable *)
  let cost a = 
    let ti = (Battlefield.get_tile m a) in 
    if (Tile.traversable_m ti move_type) then Some (Tile.movement_cost ti move_type) else None in
    
  let (w,h) = Battlefield.size m in
  
  (* table des distances à pos *)
  let dist = Array.make_matrix w h None in
  
  (* prev.(x).(y) est la position de la tuile precedant Position.create(x,y) sur le chemin de pos a Position.create(x,y) *)
  let prev = Array.make_matrix w h None in
  
  let access a p = let (a1,a2) = Position.topair p in a.(a1).(a2) in
  let change a p v = let (a1,a2) = Position.topair p in a.(a1).(a2) <- v in
  
  (* recherche de min dist dans une liste *)
  let rec min_dist a0 = function
    |[] -> a0
    |a::q -> match (access dist a,access dist a0) with 
            | None , _ -> min_dist a0 q 
            | _ , None -> min_dist a q
            | Some c , Some d -> min_dist (if c <= d then a else a0) q 
  in
  let shuffle l =
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond
  in

  (* liste des sommets non parcourus *)
  let li = ref [] in 
  
  (* initialisations *)
  change dist pos (Some 0);
  Battlefield.tile_iteri (fun p t -> if (Tile.traversable_m t move_type) then li := p::( !li)) m;
    
  (* boucle principale *)
  while !li <> [] do
    (* u : min dist a pour tout a non parcouru, i.e. plus près voisin atteignable des parcourus *)
    let u = min_dist (List.hd( !li)) (List.tl( !li)) in
      li := List.filter (fun x -> x <> u) ( !li);
      match access dist u with
      | None -> li := []
      | Some du -> (* pour tout voisin v de u atteignable d'une autre façon, on teste si c'est plus court d'aller en u par v *)
          let shorter_path v cv = 
            let alternate_cost = du + cv in 
            if match access dist v with | None -> true | Some dv -> alternate_cost < dv then
            begin
              change dist v (Some alternate_cost);
              change prev v (Some u);
            end
          in
          List.iter (fun v -> let open Utils in (cost v) >? (shorter_path v)) 
                    ( List.filter (Battlefield.in_range m) (shuffle [Position.up u; Position.down u; Position.left u; Position.right u]) );
  done;
  (* construit le chemin jusqu'à pos en remontant  prev *)
  let rec rev_path = function
    | None -> []
    | Some prev_pos -> prev_pos::(rev_path (access prev prev_pos))
  in
  (fun tar ->  
      match access dist tar with
      | None -> None
      | Some dab -> Some (dab , List.rev ( rev_path (Some tar) ) ) 
  )


