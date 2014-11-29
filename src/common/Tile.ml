open Tile_t

type structure = Tile_t.structure

type t = Tile_t.t

let get_name tile = tile.name

let get_density tile = tile.density

let get_grow_speed tile = tile.grow_speed

let get_structure tile = tile.structure

let traversable_m tile movement =
  let open Unit in
  match movement with
  | Walk  -> tile.walk_cost >= 0
  | Roll  -> tile.roll_cost >= 0
  | Tread -> tile.tread_cost >= 0
  | Swim  -> tile.swim_cost >= 0
  | Fly   -> tile.fly_cost >= 0
  | Amphibious_Walk  -> tile.swim_cost >= 0 || tile.walk_cost >= 0
  | Amphibious_Roll  -> tile.swim_cost >= 0 || tile.roll_cost >= 0
  | Amphibious_Tread -> tile.swim_cost >= 0 || tile.tread_cost >= 0
  | All   -> List.exists (fun i -> i>=0) [tile.roll_cost; tile.tread_cost; tile.swim_cost; tile.fly_cost]

let traversable tile soldier = traversable_m tile soldier#movement_type

let compare_movements_list t1 t2 li =
  let count f l = List.fold_left (fun c e -> if f e then 1+c else c) 0 l in
  let csup = count (fun i -> i>0) li in
  let cinf = count (fun i -> i<0) li in
  if csup = 0 then
    Some (-cinf)
  else
    if cinf = 0 then
      Some csup
    else
      None

let compare_movements t1 t2 = 
  compare_movements_list t1 t2 
    [ compare (t1.walk_cost >=0) (t2.walk_cost >=0) ;
      compare (t1.roll_cost >=0) (t2.roll_cost >=0) ;
      compare (t1.tread_cost >=0) (t2.tread_cost >=0) ;
      compare (t1.swim_cost >=0) (t2.swim_cost >=0) ;
      compare (t1.fly_cost >=0) (t2.fly_cost >=0) ]

let compare_walkability t1 t2 = 
  compare_movements_list t1 t2 
    [ compare (t1.walk_cost >=0) (t2.walk_cost >=0) ;
      compare (t1.roll_cost >=0) (t2.roll_cost >=0) ;
      compare (t1.tread_cost >=0) (t2.tread_cost >=0) ]

let movement_cost tile movement =
  let min_pos a b = if a >= 0 then if b >= 0 then min a b else a else b in
  let cost =
    let open Unit in
    match movement with
      | Walk  -> tile.walk_cost
      | Roll  -> tile.roll_cost
      | Tread -> tile.tread_cost
      | Swim  -> tile.swim_cost
      | Fly   -> tile.fly_cost
      | Amphibious_Walk  -> min_pos tile.swim_cost tile.walk_cost
      | Amphibious_Roll  -> min_pos tile.swim_cost tile.roll_cost
      | Amphibious_Tread -> min_pos tile.swim_cost tile.tread_cost
      | All   ->
          List.fold_left
            min_pos
            tile.walk_cost
            [tile.roll_cost; tile.tread_cost; tile.swim_cost; tile.fly_cost] in
  if cost >= 0 then cost else failwith("Tile.movement_cost : not a valid movement")

let tile_cost tile soldier = movement_cost tile soldier#movement_type

let parsed_tile_to_tile t = t

let tile_to_parsed_tile t = t

let check_cost_validity c = c >= -1

let check_density_validity d = d >= 0

let check_grow_speed_validity g = g >= 1

let check_structure_validity st =
  match st with
  | `Border (_,rate,expansion) -> ( rate >= 0 || rate <= 1000 || expansion >= 0)
  | _ -> true


