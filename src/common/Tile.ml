(* Dummy Tile implementation *)

type t = string

let get_name tile = tile

let walkable tile = true
let navigable tile = true
let flyable tile = true

(* Awful *)
let create_from_file s1 s2 = s1
