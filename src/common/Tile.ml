(* Dummy Tile implementation *)

type tile_t = 
  {
    name : string;
    walk_cost : int;
    wheels_cost : int;
    tracks_cost : int;
    swim_cost : int;
    fly_cost : int
  }
type t = tile_t

let get_name tile = tile.name

let walkable tile = tile.walk_cost >= 0
let wheelable tile = tile.wheels_cost >= 0 (*ouch*)
let trackable tile = tile.tracks_cost >= 0 (*big ouch*)
let navigable tile = tile.swim_cost >= 0
let flyable tile = tile.fly_cost >= 0

let movement_cost tile movement = 
let min_pos a b = if a >=0 then if b>=0 then min a b else a else b in
let cost = begin 
    let open Unit in
    match movement with
    | Walk -> tile.walk_cost
    | Wheels -> tile.wheels_cost
    | Tracks -> tile.tracks_cost
    | Swim -> tile.swim_cost
    | Fly -> tile.fly_cost
    | Amphibious_Walk -> min_pos tile.swim_cost tile.walk_cost
    | Amphibious_Wheels -> min_pos tile.swim_cost tile.wheels_cost
    | Amphibious_Tracks-> min_pos tile.swim_cost tile.tracks_cost
end in
if cost >=0 then cost else failwith("Tile.movement_cost : not a valid movement")

(* Awful *)
let create_from_file s1 s2 = {name = s1; walk_cost = 2; wheels_cost = 2;tracks_cost = 2; swim_cost = -1; fly_cost = 1}


