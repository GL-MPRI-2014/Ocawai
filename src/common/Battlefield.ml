
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

let in_range (bf : t) (pos : Position.t) : bool =
  let pmin = Position.create (0,0) in
  let pmax = Position.create (let w,h = size bf in w-1,h-1) in
  not (Position.out_of_bounds pos pmin pmax)
