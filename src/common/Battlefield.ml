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
  let m = Array.make_matrix 100 100 (Tile.create_from_file "water" "") in
  m.(40).(40) <- Tile.create_from_file "forest" "";
  m.(41).(40) <- Tile.create_from_file "plain" "";
  m.(39).(40) <- Tile.create_from_file "plain" "";
  m.(40).(39) <- Tile.create_from_file "plain" "";
  m.(40).(41) <- Tile.create_from_file "plain" "";
  m.(41).(41) <- Tile.create_from_file "concrete" "";
  m.(39).(39) <- Tile.create_from_file "concrete" "";
  m.(39).(41) <- Tile.create_from_file "concrete" "";
  m.(41).(39) <- Tile.create_from_file "concrete" "";
  m
