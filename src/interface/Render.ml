open OcsfmlGraphics

let texture_library = TextureLibrary.create ()

let render_tile (target : #OcsfmlGraphics.render_target) pos tile =
  let (x,y) = Position.topair pos in
  let (x,y) = (float_of_int x, float_of_int y) in
  (* Hardcoding is bad *)
  let pos = (50. *. x, 50. *. y) in
  let texture_name = Tile.get_name tile in
  let texture = TextureLibrary.get_texture texture_library texture_name in
  let sprite = new OcsfmlGraphics.sprite
    ~texture: texture
    ~position: pos
    ()
  in
  target#draw sprite

let render_map (target : #OcsfmlGraphics.render_target) (map : Battlefield.t) =
  Battlefield.tile_iteri (render_tile target) map


let () =
  TextureLibrary.load_directory texture_library "resources/textures/"
