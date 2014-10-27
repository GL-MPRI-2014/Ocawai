open Utils

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
  let cost(a,b) = 
    let ti = (Battlefield.get_tile m (Position.create (a,b))) in 
    if (Tile.traversable_m ti move_type) then Some (Tile.movement_cost ti move_type) else None in
  let (w,h) = Battlefield.size m in
  
  (* table des distances à pos *)
  let dist = Array.make_matrix w h None in
  
  (* prev.(x).(y) est la position de la tuile precedant Position.create(x,y) sur le chemin de pos a Position.create(x,y) *)
  let prev = Array.make_matrix w h None in
  
  (* recherche de min dist dans une liste *)
  let rec min_dist (a0,b0) = function
    |[] -> (a0,b0)
    |(a,b)::q when match (dist.(a).(b),dist.(a0).(b0)) with 
                  | None , _ -> false 
                  | _ , None -> true 
                  | Some c , Some d -> c <= d 
                -> min_dist (a,b) q
    |t::q -> min_dist (a0,b0) q
  in
  let remove r l = List.filter (fun x -> x <> r) l in
  (* liste des somments non parcourus *)
  let li = ref [] in 
  let (x0,y0) = Position.topair pos in
  
  (* initialisations *)
  dist.(x0).(y0) <- Some 0;
  for i = 0 to w-1 do
    for j = 0 to h-1 do 
      if (Tile.traversable_m (Battlefield.get_tile m (Position.create (i,j))) move_type) then li := (i,j)::( !li)
    done;
  done;    
  
  (* boucle principale *)
  while !li <> [] do
    (* x,y : min dist (a,b) pour tout a,b non parcourus, i.e. plus près voisin atteignable des parcourus *)
    let (x,y) = let (xh,yh) = List.hd( !li) in min_dist (xh,yh) (List.tl( !li)) in
      li := remove (x,y) ( !li);
      match dist.(x).(y) with
      | None -> li := []
      | Some dxy -> (* pour tout voisin a,b de x,y atteignable d'une autre façon, on teste si c'est plus court d'aller en x,y par a,b *)
          let shorter_path a b co = 
            let alt = dxy + co in 
            if match dist.(a).(b) with | None -> true | Some dab -> alt < dab then
            begin
              dist.(a).(b) <- Some alt;
              prev.(a).(b) <- Some (Position.create (x,y));
            end
          in
          List.iter (fun (a,b) -> cost (a,b) >? shorter_path a b) ( List.filter (fun (a,b) -> a>=0 && b>=0 && a<w && b<h) [(x,y+1);(x,y-1);(x+1,y);(x-1,y)] );
  done;
  (* construit le chemin jusqu'à pos en remontant  prev *)
  let rec rev_path = function
    | None -> []
    | Some prev_pos ->let (a,b) = Position.topair prev_pos in prev_pos::(rev_path prev.(a).(b))
  in
  (fun tar -> 
      let a,b = Position.topair tar in 
      match dist.(a).(b) with
      | None -> None
      | Some dab -> Some (dab , List.rev ( rev_path (Some tar) ) ) 
  )


