
type t = Tile_t.t

let get_name tile = tile.Tile_t.name

let walkable tile = tile.Tile_t.walk_cost >= 0
let rollable tile = tile.Tile_t.roll_cost >= 0 (*ouch*)
let trackable tile = tile.Tile_t.tracks_cost >= 0 (*big ouch*)
let navigable tile = tile.Tile_t.swim_cost >= 0
let flyable tile = tile.Tile_t.fly_cost >= 0

let traversable_m tile movement =
let open Unit in
match movement with
| Walk -> tile.Tile_t.walk_cost >= 0
| Roll -> tile.Tile_t.roll_cost >= 0
| Tracks -> tile.Tile_t.tracks_cost >= 0
| Swim -> tile.Tile_t.swim_cost >= 0
| Fly -> tile.Tile_t.fly_cost >= 0
| Amphibious_Walk -> tile.Tile_t.swim_cost >= 0 || tile.Tile_t.walk_cost >= 0
| Amphibious_Roll -> tile.Tile_t.swim_cost >= 0 || tile.Tile_t.roll_cost >= 0
| Amphibious_Tracks-> tile.Tile_t.swim_cost >= 0 || tile.Tile_t.tracks_cost >= 0

let traversable tile soldier = traversable_m tile soldier#movement_type

let movement_cost tile movement = 
let min_pos a b = if a >=0 then if b>=0 then min a b else a else b in
let cost = begin 
    let open Unit in
    match movement with
    | Walk -> tile.Tile_t.walk_cost
    | Roll -> tile.Tile_t.roll_cost
    | Tracks -> tile.Tile_t.tracks_cost
    | Swim -> tile.Tile_t.swim_cost
    | Fly -> tile.Tile_t.fly_cost
    | Amphibious_Walk -> min_pos tile.Tile_t.swim_cost tile.Tile_t.walk_cost
    | Amphibious_Roll -> min_pos tile.Tile_t.swim_cost tile.Tile_t.roll_cost
    | Amphibious_Tracks-> min_pos tile.Tile_t.swim_cost tile.Tile_t.tracks_cost
end in
if cost >=0 then cost else failwith("Tile.movement_cost : not a valid movement")

let tile_cost tile soldier = movement_cost tile soldier#movement_type

let create_from_file s1 s2 =
  List.find
    (fun tile -> tile.Tile_t.name = s1)
    (Ag_util.Json.from_file Tile_j.read_t_list "resources/config/tiles.json"(*s2*))
  (*TODO : cache *)
