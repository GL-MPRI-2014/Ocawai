open OcsfmlGraphics

let foi2D (a,b) = (float_of_int a, float_of_int b)
let subf2D (a,b) (c,d) = (a-.c,b-.d)
let (>?) opt f = match opt with
  |None -> ()
  |Some(s) -> f s

let texture_library = TextureLibrary.create ()
let font = new font `None

let draw_texture (target : #OcsfmlGraphics.render_target) camera pos rot name =
  let texture = TextureLibrary.get_texture texture_library name in
  let (sx,sy) =  foi2D texture#get_size in
  let origin = (sx/.2.,sy/.2.) in
  let position = foi2D (camera#project pos) in
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
  let ts = float_of_int camera#tile_size in
  new rectangle_shape
    ~size:(ts -. 2., ts -. 2.)
    ~position
    ~origin:(ts /. 2., ts /. 2.)
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
    (Position.square camera#top_left camera#bottom_right)


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
      let ap = angle pos prev
      and an = angle pos next in
      let (amax,amin) = if an > ap then (an,ap) else (ap,an) in
      if amax = amin +. 180. then
        draw pos ap "arrow_straight"
      else begin
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


(* This is not-so-garbage *)
let draw_unit (target : #OcsfmlGraphics.render_target) camera my_unit =
  (* We might later want to draw it a bit to the top *)
    draw_texture target camera (Unit.get_position my_unit)
      0. (Unit.get_name my_unit)


(* This is almost garbage *)
let draw_range (target : #OcsfmlGraphics.render_target) camera my_unit =
  let move_range = Position.filled_circle
      (Unit.get_position my_unit)
      (Unit.move_range my_unit)
  in
  let attack_range = ref [] in
  for i = 1 to Unit.attack_range my_unit do
    attack_range := !attack_range @
      (Position.neighbours (!attack_range @ move_range))
  done;
  List.iter (highlight_tile target camera Color.yellow) move_range;
  List.iter (highlight_tile target camera Color.red) !attack_range


(* Draw the cursor *)
let draw_cursor (target : #OcsfmlGraphics.render_target) (camera : Camera.camera) =
  let texture = TextureLibrary.get_texture texture_library "cursor" in
  let (sx,sy) =  foi2D texture#get_size in
  let origin = (sx/.2.,sy/.2.) in
  let position = foi2D (camera#project camera#cursor#position) in
  let scale = camera#cursor#scale in
  new OcsfmlGraphics.sprite
    ~texture
    ~position
    ~origin
    ~scale:(scale, scale)
    ()
  |> target#draw



(* Problem : Currently the text position depends of the resolution *)
(* We need to think it through not to have weird effects depending *)
(* on the screen *)
let draw_hud (target : #OcsfmlGraphics.render_target) =
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


let render_game (target : #OcsfmlGraphics.render_target) data =
  render_map target data#camera data#map;
  data#selected >? draw_range target data#camera;
  draw_path target data#camera data#current_move;
  draw_cursor target data#camera;
  List.iter (draw_unit target data#camera) data#units;
  FPS.display target


let () =
  TextureLibrary.load_directory texture_library "resources/textures/" ;
  if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
  then failwith "Couldn't load the font"
