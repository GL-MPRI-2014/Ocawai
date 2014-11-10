open Tile_t

type structure = [ `Block | `Road | `Border of string ]

type t = Tile_t.t

let get_name tile = tile.name

let get_density tile = tile.density

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
  | All   -> true

let traversable tile soldier = traversable_m tile soldier#movement_type

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
            [tile.roll_cost; tile.tread_cost; tile.swim_cost;
           tile.fly_cost] in
  if cost >= 0 then cost else failwith("Tile.movement_cost : not a valid movement")

let tile_cost tile soldier = movement_cost tile soldier#movement_type

let create_list_from_file file =
  Ag_util.Json.from_file Tile_j.read_t_list file
  (*TODO : cache ? *)

let create_from_file name file =
  let tiles = create_list_from_file file in
  List.find
    (fun t -> t.name = name)
    tiles

let create_list_from_config () = create_list_from_file "resources/config/tiles.json"

let create_from_config name = create_from_file name "resources/config/tiles.json"

