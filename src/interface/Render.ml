open OcsfmlGraphics

let texture_library = TextureLibrary.create ()
let font = new font `None

let render_tile (target : #OcsfmlGraphics.render_target) pos tile =
  let (x,y) = Position.topair pos in
  let (x,y) = (float_of_int x, float_of_int y) in
  (* Hardcoding is bad *)
  let position = (50. *. x, 50. *. y) in
  let texture_name = Tile.get_name tile in
  let texture = TextureLibrary.get_texture texture_library texture_name in
  let sprite = new OcsfmlGraphics.sprite
    ~texture
    ~position
    ()
  in
  target#draw sprite

let highlight_tile (target : #OcsfmlGraphics.render_target) base_color pos =
  let (x,y) = Position.topair pos in
  let (r,g,b) = Color.(base_color.r, base_color.g, base_color.b) in
  (* Hardcoding is REALLY bad *)
  let position = (50. *. (float_of_int x), 50. *. (float_of_int y)) in
  new rectangle_shape
    ~size:(48.,48.)
    ~position
    ~fill_color:(Color.rgba r g b 140)
    ~outline_color:(Color.rgba 255 255 255 180)
    ~outline_thickness:2.
    ()
  (* Additive blending is not so good finally, so I will stick with the good ol'
   * default blending *)
  |> target#draw


let render_map (target : #OcsfmlGraphics.render_target) (map : Battlefield.t) =
  Battlefield.tile_iteri (render_tile target) map;

  (* Some tests *)
  let circle = Position.filled_circle (Position.create (3,3)) 2 in
  List.iter (highlight_tile target Color.yellow) circle;
  List.iter (highlight_tile target Color.red) (Position.neighbours circle) ;

  (* Some others *)
  let text : text = new text
    ~string:"PingouinSetter's turn"
    ~font
    ~character_size:20
    ~color:Color.white
    ()
  in
  let (w,h) = target#get_size in
  let (w,h) = float_of_int w, float_of_int h in
  let text_width = text#get_global_bounds.width in
  text#set_position ((w -. text_width) /. 2.) (h -. 60.);
  target#draw text


let () =
  TextureLibrary.load_directory texture_library "resources/textures/" ;
  if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
  then failwith "Couldn't load the font"
