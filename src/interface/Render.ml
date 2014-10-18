open OcsfmlGraphics

let foi2D (a,b) = (float_of_int a, float_of_int b)
let subf2D (a,b) (c,d) = (a-.c,b-.d)

let texture_library = TextureLibrary.create ()
let font = new font `None


let draw_texture (target : #OcsfmlGraphics.render_target) camera pos rot name =
  let texture = TextureLibrary.get_texture texture_library name in
  let (sx,sy) =  foi2D texture#get_size in
  let origin = (sx/.2.,sy/.2.) in
  let position = subf2D (foi2D (camera#project pos)) origin in
  let rotation = rot in
  new OcsfmlGraphics.sprite
    ~texture
    ~position
    ~rotation
    ~origin
    ()
  |> target#draw

let render_tile (target : #OcsfmlGraphics.render_target) camera pos tile =
  let texture_name = Tile.get_name tile in
  draw_texture target camera pos 0. texture_name

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
  (* This would have to be drawn somewhere else later *)
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

  let angle s t =
    match Position.diff t s with
      | pos when pos = Position.create (1,0)  -> 0.
      | pos when pos = Position.create (0,1)  -> 90.
      | pos when pos = Position.create (-1,0) -> 180.
      | pos when pos = Position.create (0,-1) -> -90.
      | _ -> failwith "Not continuous path"
  in

  let rec aux prev = function
    | pos :: [] -> draw pos (angle prev pos) "arrow_end"
    | pos :: next :: r ->
      let ap = angle prev pos
      and an = angle pos next in
      if ap = an then
        draw pos ap "arrow_straight"
      else begin
        let (amax,amin) = if an > ap then (an,ap) else (ap,an) in
        if amax = amin +. 270. then
          draw pos 270. "arrow_corner"
        else
          draw pos amax "arrow_corner"
      end ;
      aux pos (next :: r)
    | [] -> ()
  in

  match path with
    | start :: [] -> draw start 0. "arrow_lone"
    | start :: next :: r ->
      draw start (angle start next) "arrow_start" ;
      aux start (next :: r)
    | [] -> ()


let () =
  TextureLibrary.load_directory texture_library "resources/textures/" ;
  if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
  then failwith "Couldn't load the font"
