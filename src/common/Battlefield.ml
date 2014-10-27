
type t = Tile.t array array

let create =
  Array.make_matrix

let get_tile m pos =
  let (x,y) = Position.topair pos in
  m.(x).(y)

let set_tile m pos tile =
  let (x,y) = Position.topair pos in
  m.(x).(y) <- tile

let tile_iter f m =
  Array.iter (Array.iter f) m

let tile_iteri f m =
  Array.iteri (fun x -> Array.iteri (fun y -> f (Position.create (x,y)))) m

let size m = (Array.length m,Array.length m.(0))

