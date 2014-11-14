open OcsfmlGraphics
open Utils

let texture_library = TextureLibrary.create ()
let font = new font `None

let filter_positions map p =
  not (Position.out_of_bounds p
        (Position.create (0,0))
        (Position.diff
          (Position.create (Battlefield.size map))
          (Position.create (1,1))))

let draw_txr (target : #OcsfmlGraphics.render_target) name position ?rotation
  ?scale ?blend_mode ?color () =
    let texture = TextureLibrary.get_texture texture_library name in
    match texture with
    |TextureLibrary.Tex(texture) ->
      let (sx,sy) =  foi2D texture#get_size in
      let origin  = (sx/.2.,sy/.2.) in
      new sprite ~origin ~position ?rotation ~texture ?scale ?color ()
      |> target#draw ?blend_mode
    |_ -> assert false

let draw_tile (target : #OcsfmlGraphics.render_target) tileset_name tile_name 
  position ?rotation ?scale ?blend_mode ?color () = 
    let texture = TextureLibrary.get_texture texture_library tileset_name in
    match texture with
    |TextureLibrary.TSet(set) ->
      let (sx,sy) = foi2D (set#tile_size, set#tile_size) in 
      let origin  = (sx/.2., sy/.2.) in
      new sprite ~origin ~position ?rotation ~texture:set#texture
        ~texture_rect:(set#texture_rect tile_name) ?scale ?color ()
      |> target#draw ?blend_mode
    | _ -> assert false

let draw_from_map (target : #OcsfmlGraphics.render_target) camera 
  name position ?rotation ?offset:(offset = (0.,0.)) 
  ?scale:(scale = (1.,1.)) ?blend_mode ?color () = 
  let (ox,oy) = offset in
  let position = addf2D
    (foi2D (camera#project position))
    (ox *. camera#zoom, oy *. camera#zoom)
  in
  let scale = (fst scale *. camera#zoom, snd scale *. camera#zoom) in
  draw_txr target name position ?color ?rotation ~scale ?blend_mode ()

let draw_tile_from_map (target : #OcsfmlGraphics.render_target) camera 
  set_name tile_name position ?rotation ?offset:(offset = (0.,0.)) 
  ?scale:(scale = (1.,1.)) ?blend_mode ?color () = 
  let (ox,oy) = offset in
  let position = addf2D
    (foi2D (camera#project position))
    (ox *. camera#zoom, oy *. camera#zoom)
  in
  let scale = (fst scale *. camera#zoom, snd scale *. camera#zoom) in
  draw_tile target set_name tile_name position 
  ?color ?rotation ~scale ?blend_mode ()

let render_joint (target : #OcsfmlGraphics.render_target) camera pos tile_name map =
  (* Utility *)
  let draw_v = draw_from_map target camera ~offset:(0.,25.) in
  let draw_h = draw_from_map target camera ~offset:(25.,0.) in
  let texture_name = tile_name in
  (* Hardcode for testing *)
  (* Let's draw the junction *)
  let up = Position.up pos in
  let left = Position.left pos in
  let is_ground name =
    name = "plain" || name = "forest" || name = "mountain"
  in
  if filter_positions map up then
  begin
    let upname = Tile.get_name (Battlefield.get_tile map up) in
    if texture_name = "water" && is_ground upname then
      draw_v "ground_water_v" up ~rotation:0. ()
    else if is_ground texture_name && upname = "water" then
      draw_v "water_ground_v" up ~rotation:180. ()
  end ;
  if filter_positions map left then
  begin
    let leftname = Tile.get_name (Battlefield.get_tile map left) in
    if texture_name = "water" && is_ground leftname then
      draw_h "water_ground_h" left ~rotation:180. ()
    else if is_ground texture_name && leftname = "water" then
      draw_h "water_ground_h" left ~rotation:0. ()
  end

let highlight_tile (target : #OcsfmlGraphics.render_target) camera
                   base_color pos =
  let (r,g,b,a) = Color.(
    base_color.r, base_color.g, base_color.b, 
    float_of_int (base_color.a)) in
  let multiplier = sin (Unix.gettimeofday () *. 2.) +. 2. in 
  let alpha = int_of_float ((multiplier /. 3.) *. a) in 
  draw_from_map target camera "highlight" pos ~color:(Color.rgba r g b alpha)
    ~blend_mode:BlendAdd ()

let render_map (target : #OcsfmlGraphics.render_target) camera
               (map : Battlefield.t) =
  let render_tile n p = 
    draw_tile_from_map target camera "tileset" n p ();
    render_joint target camera p n map
  in
  List.iter
    (fun p -> render_tile (Tile.get_name (Battlefield.get_tile map p)) p)
    (Position.square camera#top_left camera#bottom_right)

let draw_path (target : #OcsfmlGraphics.render_target) camera path =
  let draw = draw_from_map target camera in
  let angle s t =
    match Position.diff t s with
      | pos when pos = Position.create (1,0)  -> 0.
      | pos when pos = Position.create (0,1)  -> 90.
      | pos when pos = Position.create (-1,0) -> 180.
      | pos when pos = Position.create (0,-1) -> -90.
      | _ -> failwith "Not continuous path"
  in
  let rec aux prev = function
    | pos :: [] -> draw "arrow_end" pos ~rotation:(angle prev pos) ()
    | pos :: next :: r ->
      let ap = angle pos prev
      and an = angle pos next in
      let (amax,amin) = if an > ap then (an,ap) else (ap,an) in
      if amax = amin +. 180. then
        draw "arrow_straight" pos ~rotation:ap ()
      else begin
        if amax = amin +. 270. then
          draw "arrow_corner" pos ~rotation:270. ()
        else
          draw "arrow_corner" pos ~rotation:amax ()
      end ;
      aux pos (next :: r)
    | [] -> ()
  in
  match path with
    | start :: [] -> draw "arrow_lone" start ()
    | start :: next :: r ->
      draw "arrow_start" start ~rotation:(angle start next) ();
      aux start (next :: r)
    | [] -> ()


(* This is not-so-garbage *)
let draw_unit (target : #OcsfmlGraphics.render_target) camera my_unit =
  (* We might later want to draw it a bit to the top *)
  draw_from_map target camera (my_unit#name) (my_unit#position) ()

let draw_range (target : #OcsfmlGraphics.render_target) camera map =
  match camera#cursor#get_state with
  |Cursor.Idle -> ()
  |Cursor.Displace(_,_,(range,_)) -> begin
    List.iter (highlight_tile target camera (Color.rgba 255 255 100 150)) range
  end
  |Cursor.Action(my_unit, pos) -> begin
    let attack_range = 
      List.filter (filter_positions map)
      (Position.range pos 1 my_unit#attack_range)
    in
    List.iter (highlight_tile target camera (Color.rgba 255 50 50 255)) attack_range
  end

(* Draw the cursor *)
let draw_cursor (target : #OcsfmlGraphics.render_target) (camera : Camera.camera) =
  draw_from_map target camera "cursor" camera#cursor#position 
  ~offset:(Utils.subf2D (0.,0.) camera#cursor#offset) 
  ~scale:(camera#cursor#scale, camera#cursor#scale) ()


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


let draw_gui (target : #OcsfmlGraphics.render_target) ui_manager =
  ui_manager#draw (target :> render_target) texture_library


let render_game (target : #OcsfmlGraphics.render_target)
  (data : ClientData.client_data) =
  render_map target data#camera data#map;
  draw_range target data#camera data#map;
  draw_path target data#camera data#current_move;
  draw_cursor target data#camera;
  List.iter (fun p -> List.iter (draw_unit target data#camera) p#get_army)
    data#players;
  FPS.display target

let load_ressources () =
  TextureLibrary.load_directory texture_library "resources/textures/" ;
  if not (font#load_from_file "resources/fonts/Roboto-Regular.ttf")
  then failwith "Couldn't load the font"
