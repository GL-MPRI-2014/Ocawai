open OcsfmlGraphics

let foi2D (a,b) = (float_of_int a, float_of_int b)

let texture_library = TextureLibrary.create ()
let font = new font `None


let draw_texture (target : #OcsfmlGraphics.render_target) camera pos name =
  let position = foi2D (camera#project pos) in
  let texture = TextureLibrary.get_texture texture_library name in
  let sprite = new OcsfmlGraphics.sprite
    ~texture
    ~position
    ()
  in
  target#draw sprite

let render_tile (target : #OcsfmlGraphics.render_target) camera pos tile =
  let texture_name = Tile.get_name tile in
  draw_texture target camera pos texture_name

let highlight_tile (target : #OcsfmlGraphics.render_target) camera
                   base_color pos =
  let (r,g,b) = Color.(base_color.r, base_color.g, base_color.b) in
  let position = foi2D (camera#project pos) in
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


let render_map (target : #OcsfmlGraphics.render_target) camera
               (map : Battlefield.t) =

  List.iter
    (fun p -> render_tile target camera p (Battlefield.get_tile map p))
    (Position.square camera#top_left camera#bottom_right);

(*   Battlefield.tile_iteri (render_tile target camera) map; *)

  (* Some tests *)
  let circle = Position.filled_circle camera#cursor 2 in
  List.iter (highlight_tile target camera Color.yellow) circle;
  List.iter (highlight_tile target camera Color.red)
    (Position.neighbours circle) ;

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


let draw_path (target : #OcsfmlGraphics.render_target) camera path =

  let draw = draw_texture target camera in

  let rec aux = function
    | pos :: [] -> draw pos "arrow_end"
    | pos :: r ->
      draw pos "arrow_straight" ; aux r
    | [] -> ()
  in

  draw (List.hd path) "arrow_start" ;
  aux (List.tl path)


let () =
  TextureLibrary.load_directory texture_library "resources/textures/" ;
  if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
  then failwith "Couldn't load the font"
