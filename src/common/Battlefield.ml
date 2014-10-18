(** This file is not meant to be pushed on master.                           **)
(** It is only built for testing purpose while the real one is not ready.    **)

type t = Tile.t array array

let create =
  Array.make_matrix

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
  let m = Array.make_matrix 10 10 (Tile.create_from_file "water" "") in
  m.(4).(4) <- Tile.create_from_file "forest" "";
  m.(5).(4) <- Tile.create_from_file "plain" "";
  m.(3).(4) <- Tile.create_from_file "plain" "";
  m.(4).(3) <- Tile.create_from_file "plain" "";
  m.(4).(5) <- Tile.create_from_file "plain" "";
  m.(5).(5) <- Tile.create_from_file "concrete" "";
  m.(3).(3) <- Tile.create_from_file "concrete" "";
  m.(3).(5) <- Tile.create_from_file "concrete" "";
  m.(5).(3) <- Tile.create_from_file "concrete" "";
  m
