(* Dummy Tile implementation *)

type tile_t = 
  {
    name : string
    walkable : bool;
    navigable : bool;
    flyable : bool;
    cost : int;
  }
type t = tile_t

let get_name tile = tile.string

let walkable tile = tile.walkable
let navigable tile = tile.navigable
let flyable tile = tile.flyable

let movement_cost tile mouvement = tile.cost

(* Awful *)
let create_from_file s1 s2 = s1
