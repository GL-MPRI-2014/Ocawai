(** This file is not meant to be pushed on master.                           **)
(** It is only built for testing purpose while the real one is not ready.    **)

type t = Tile.t array array

let get_tile map pos =
  let (x,y) = Position.topair pos in
  map.(x).(y)

let set_tile map pos tile =
  let (x,y) = Position.topair pos in
  map.(x).(y) <- tile

let tile_iter f map =
  Array.iter (Array.iter f) map

let tile_iteri f map =
  Array.iteri (fun x -> Array.iteri (fun y -> f (Position.create (x,y)))) map

let dummy_map () =
  Array.make_matrix 10 10 (Tile.create_from_file "water.png" "")
