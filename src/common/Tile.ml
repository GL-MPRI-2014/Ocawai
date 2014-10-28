
type t = {
  name: string;
  walk_cost: int;
  roll_cost: int;
  tread_cost: int;
  swim_cost: int;
  fly_cost: int;
  density: int
}

let get_name tile = tile.name

let get_density tile = tile.density

let traversable_m tile movement =
let open Unit in
match movement with
| Walk -> tile.walk_cost >= 0
| Roll -> tile.roll_cost >= 0
| Tread -> tile.tread_cost >= 0
| Swim -> tile.swim_cost >= 0
| Fly -> tile.fly_cost >= 0
| Amphibious_Walk -> tile.swim_cost >= 0 || tile.walk_cost >= 0
| Amphibious_Roll -> tile.swim_cost >= 0 || tile.roll_cost >= 0
| Amphibious_Tread-> tile.swim_cost >= 0 || tile.tread_cost >= 0

let traversable tile soldier = traversable_m tile soldier#movement_type

let movement_cost tile movement =
let min_pos a b = if a >=0 then if b>=0 then min a b else a else b in
let cost = begin
  let open Unit in
  match movement with
  | Walk -> tile.walk_cost
  | Roll -> tile.roll_cost
  | Tread -> tile.tread_cost
  | Swim -> tile.swim_cost
  | Fly -> tile.fly_cost
  | Amphibious_Walk -> min_pos tile.swim_cost tile.walk_cost
  | Amphibious_Roll -> min_pos tile.swim_cost tile.roll_cost
  | Amphibious_Tread-> min_pos tile.swim_cost tile.tread_cost
end in
if cost >=0 then cost else failwith("Tile.movement_cost : not a valid movement")

let tile_cost tile soldier = movement_cost tile soldier#movement_type

let tile_t_to_t ti = {name = ti.Tile_t.name;
  walk_cost = ti.Tile_t.walk_cost;
  roll_cost = ti.Tile_t.roll_cost;
  tread_cost = ti.Tile_t.tread_cost;
  swim_cost = ti.Tile_t.swim_cost;
  fly_cost = ti.Tile_t.fly_cost;
  density = ti.Tile_t.density}

let create_from_file s1 s2 =
  let ti =List.find
    (fun tile -> tile.Tile_t.name = s1)
    (Ag_util.Json.from_file Tile_j.read_t_list s2) in
  tile_t_to_t ti
  (*TODO : cache *)


let create_from_config s1 = create_from_file s1 "resources/config/tiles.json"
