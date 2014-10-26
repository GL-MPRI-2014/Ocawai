
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

let print m = 
Array.iter (fun t -> (Array.iter (fun tt -> 
  print_string (match (Tile.get_name tt) with
              | "water" -> "  "
              | "plain" -> ".."
              | "forest" -> "::"
              | "concrete" -> "=="
              | "mountain" -> "MM"
              | _ -> "?"
  )) t;print_endline "")) m

let dummy_map () =

let ti_list = Ag_util.Json.from_file Tile_j.read_t_list "resources/config/tiles.json" in
  let tiles = List.map (fun ti -> Tile.tile_t_to_t ti) ti_list in
  let tile a = List.find (fun ti -> Tile.get_name ti = a) tiles in
  let m = create 100 100 (tile "water") in
  m.(40).(40) <- tile "forest";
  m.(41).(40) <- tile "plain";
  m.(39).(40) <- tile "plain";
  m.(40).(39) <- tile "plain";
  m.(40).(41) <- tile "plain";
  m.(41).(41) <- tile "concrete";
  m.(39).(39) <- tile "concrete";
  m.(39).(41) <- tile "concrete";
  m.(41).(39) <- tile "concrete";
  m.(35).(34) <- tile "plain";
  m.(35).(33) <- tile "plain";
  m.(34).(35) <- tile "plain";
  m.(34).(32) <- tile "plain";
  m.(33).(35) <- tile "plain";
  m.(33).(34) <- tile "plain";
  m.(33).(33) <- tile "plain";
  m.(33).(32) <- tile "plain";
  m.(31).(35) <- tile "plain";
  m.(31).(34) <- tile "plain";
  m.(31).(32) <- tile "plain";
  m.(29).(34) <- tile "plain";
  m.(29).(33) <- tile "plain";
  m.(29).(32) <- tile "plain";
  m.(28).(35) <- tile "plain";
  m.(28).(34) <- tile "plain";
  m.(27).(34) <- tile "plain";
  m.(27).(33) <- tile "plain";
  m.(27).(32) <- tile "plain";
  m.(25).(35) <- tile "plain";
  m.(25).(34) <- tile "plain";
  m.(25).(33) <- tile "plain";
  m.(25).(32) <- tile "plain";
  m.(24).(34) <- tile "plain";
  m.(24).(32) <- tile "plain";
  m.(23).(35) <- tile "plain";
  m.(23).(34) <- tile "plain";
  m.(23).(33) <- tile "plain";
  m.(23).(32) <- tile "plain";
  m.(21).(34) <- tile "plain";
  m.(21).(33) <- tile "plain";
  m.(20).(35) <- tile "plain";
  m.(20).(32) <- tile "plain";
  m.(19).(35) <- tile "plain";
  m.(19).(34) <- tile "plain";
  m.(19).(33) <- tile "plain";
  m.(19).(32) <- tile "plain";
  m
