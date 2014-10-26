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
  
let dijkstra m pos move_type =

  (* renvoie le cout de la tuile en Position.create(a,b) *)
  let cost(a,b) = 
    let ti = (Battlefield.get_tile m (Position.create (a,b))) in 
    if (Tile.traversable_m ti move_type) then Tile.movement_cost ti move_type else max_int in
  let (w,h) = Battlefield.size m in
  
  (* table des distances à pos *)
  let dist = Array.make_matrix w h max_int in
  
  (* prev.(x).(y) est la position de la tuile precedant Position.create(x,y) sur le chemin de pos a Position.create(x,y) *)
  let prev = Array.make_matrix w h None in
  
  (* recherche de min dist dans une liste *)
  let rec min_dist (a0,b0) = function
    |[] -> (a0,b0)
    |(a,b)::q when dist.(a).(b) <= dist.(a0).(b0) -> min_dist (a,b) q
    |t::q -> min_dist (a0,b0) q
  in
  let remove r l = List.filter (fun x -> x <> r) l in
  (* liste des somments non parcourus *)
  let li = ref [] in 
  let (x0,y0) = Position.topair pos in
  
  (* initialisations *)
  dist.(x0).(y0) <- 0;
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      li := (i,j)::( !li);
    done;
  done;    
  
  (* boucle principale *)
  while !li <> [] do
    (* x,y : min dist (a,b) pour tout a,b non parcourus, i.e. plus près voisin atteignable des parcourus *)
    let (x,y) = let (xh,yh) = List.hd( !li) in min_dist (xh,yh) (List.tl( !li)) in
      li := remove (x,y) ( !li);
      if dist.(x).(y) <> max_int then
        List.iter (fun (a,b) -> 
              let co = cost (a,b) in
              if co <> max_int then
                (* pour tout voisin a,b de x,y atteignable d'une autre façon, on teste si c'est plus court d'aller en x,y par a,b *)
                let alt = dist.(x).(y) + co in 
                if alt < dist.(a).(b) then
                begin
                  dist.(a).(b) <- alt;
                  prev.(a).(b) <- Some (Position.create (x,y));
                end
              ) ( List.filter (fun (a,b) -> a>=0 && b>=0 && a<w && b<h) [(x,y+1);(x,y-1);(x+1,y);(x-1,y)] );
  done;
  (* construit le chemin jusqu'à pos en remontant  prev *)
  let rec rev_path = function
    | None -> []
    | Some prev_pos ->let (a,b) = Position.topair prev_pos in prev_pos::(rev_path prev.(a).(b))
  in
  (fun tar -> 
      let a,b = Position.topair tar in 
      if dist.(a).(b) <> max_int 
        then Some (dist.(a).(b) , List.rev ( rev_path (Some tar) ) ) 
        else None
  )


