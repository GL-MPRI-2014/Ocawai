(* Dummy Tile implementation *)

type t = string

let get_name tile = tile

let walkable tile = true
let navigable tile = true
let flyable tile = true

let movement_cost t m = 1

(* Awful *)
let create_from_file s1 s2 = s1
