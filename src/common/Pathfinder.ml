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
  let cost(a,b) = 
    let ti = (Battlefield.get_tile m (Position.create (a,b))) in 
    if (Tile.traversable_m ti move_type) then Tile.movement_cost ti move_type else max_int in
  let (w,h) = Battlefield.size m in
  let dist = Array.make_matrix w h max_int in
  let prev = Array.make_matrix w h (Position.create(-1,-1)) in
  let rec min_dist r (a0,b0) = function
    |[] -> (a0,b0)
    |(a,b)::q when dist.(a).(b) <= r -> min_dist (dist.(a).(b)) (a,b) q
    |t::q -> min_dist r (a0,b0) q
  in
  let remove r l = List.filter (fun x -> x <> r) l in
  let li = ref [] in 
  let (x0,y0) = Position.topair pos in
  dist.(x0).(y0) <- 0;
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      if (i,j) <> (x0,y0) then li := (i,j)::(!li);
    done
  done;
  
  li := (x0,y0)::( !li);
  while !li <> [] do
    let (x,y) =
      let (xh,yh) = List.hd (!li) in
      min_dist (dist.(xh).(yh)) (xh,yh) (List.tl (!li)) in
    li := remove (x,y) (!li);
    if dist.(x).(y) <> max_int then
      List.iter
        (fun (a,b) -> 
              let co = cost(a,b) in
              if co <> max_int then
                let alt = dist.(x).(y) + co in 
                if alt < dist.(a).(b) then
                begin
                  dist.(a).(b) <- alt;
                  prev.(a).(b) <- Position.create (x,y);
                end)
        (List.filter
          (fun (a,b) -> a>=0 && b>=0 && a<w && b<h)
          [(x,y+1);(x,y-1);(x+1,y);(x-1,y)]);
  done;

  let rec rev_path = function
    | (-1,-1) -> []
    | (a,b) -> (Position.create (a,b))::(rev_path (Position.topair prev.(a).(b)))
  in
  (fun tar ->
    let a,b = Position.topair tar in
    (dist.(a).(b),List.rev (rev_path (a,b))))


