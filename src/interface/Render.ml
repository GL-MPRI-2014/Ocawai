open OcsfmlGraphics
open Utils
open Tileset

let renderer = object(self)

  val texture_library = TextureLibrary.create ()

  val tileset_library = TilesetLibrary.create ()

  val font = new font `None

  val mutable rect_vao = new vertex_array ~primitive_type:Quads []

  method init = 
    TextureLibrary.load_directory texture_library "resources/textures/";
    TilesetLibrary.load_directory tileset_library "resources/textures/";
    (* Recreate-it after having initialized the window *)
    rect_vao <- new vertex_array ~primitive_type:Quads []

  (* Returns true iff p is in the map *)
  method private filter_positions map p =
    not (Position.out_of_bounds p
        (Position.create (0,0))
        (Position.diff
          (Position.create (Battlefield.size map))
          (Position.create (1,1))))

  (* Draw a tile from a set, using a VAO *)
  method private draw_tile set tilename 
    ?position:(position = (0.,0.)) 
    ?scale:(scale = 1.,1.) 
    ?color:(color = Color.rgb 255 255 255) 
    ?origin:(origin = 0.,0.) () =
    let vao = set#vao in
    let tex_size = float_of_int set#tile_size in
    let new_origin = (fst scale *. fst origin, snd scale *. snd origin) in
    let real_pos = Utils.subf2D position new_origin in
    let real_end = (fst scale *. tex_size, snd scale *. tex_size) in
    let texture_rect = set#texture_rect tilename in
    vao#append (mk_vertex 
      ~position:real_pos 
      ~color 
      ~tex_coords:(texture_rect.xmin, texture_rect.ymin) ());
    vao#append (mk_vertex
      ~position:(addf2D real_pos (fst real_end, 0.))
      ~color
      ~tex_coords:(texture_rect.xmax, texture_rect.ymin) ());
    vao#append (mk_vertex
      ~position:(addf2D real_pos real_end)
      ~color
      ~tex_coords:(texture_rect.xmax, texture_rect.ymax) ());
    vao#append (mk_vertex
      ~position:(addf2D real_pos (0., snd real_end))
      ~color
      ~tex_coords:(texture_rect.xmin, texture_rect.ymax) ())


  (* Draw a texture *)
  method draw_txr (target : render_window) name 
    ?position 
    ?rotation 
    ?scale:(scale = (1.,1.)) 
    ?size 
    ?color 
    ?centered:(centered = true)
    ?blend_mode () =
    let texture = TextureLibrary.get_texture texture_library name in
    let (sx,sy) =  foi2D texture#get_size in
    let origin  = 
      if centered then (sx/.2.,sy/.2.)
      else (0.,0.)
    in
    let scale   = 
      match size with
      |None -> scale
      |Some(s) ->
        let scalem  = (fst s /. sx, snd s /. sy) in
        (fst scale *. fst scalem, snd scale *. snd scalem)
    in
    new sprite ~origin ?position ?rotation ~texture ~scale ?color ()
    |> target#draw ?blend_mode


  (* Draw a sprite from a map position and an offset *)
  method private draw_from_map 
    (target : render_window) camera name position 
    ?rotation 
    ?offset:(offset = (0.,0.)) 
    ?scale:(scale = (1.,1.)) 
    ?blend_mode 
    ?color () =
    let (ox,oy) = offset in
    let position = addf2D
      (foi2D (camera#project position))
      (ox *. camera#zoom, oy *. camera#zoom)
    in
    let scale = (fst scale *. camera#zoom, snd scale *. camera#zoom) in
    self#draw_txr target name ~position ?color 
      ?rotation ~scale ?blend_mode ()

  (* Draw a tile from a map position and an offset *)
  method private draw_tile_from_map camera set name position 
    ?offset:(offset = (0.,0.)) 
    ?scale:(scale = (1.,1.)) 
    ?color () =
    let position = addf2D
      (foi2D (camera#project position))
      ((fst offset) *. camera#zoom, (snd offset) *. camera#zoom)
    in
    let o = float_of_int set#tile_size /. 2. in
    let scale = (fst scale *. camera#zoom, snd scale *. camera#zoom) in
    self#draw_tile set name ~position ?color ~scale ~origin:(o,o) ()

  (* Render the joints *)
  method private render_joints camera jointset pos texture_name map = 
    (* Utility *)
    let draw_v = self#draw_tile_from_map camera jointset ~offset:(0.,48.) in
    let draw_h = self#draw_tile_from_map camera jointset ~offset:(50.,0.) in
    (* Hardcode for testing *)
    (* Let's draw the junction *)
    let up = Position.up pos in
    let left = Position.left pos in
    let is_ground name =
      name = "plain" || name = "forest" || name = "mountain"
    in
    if self#filter_positions map up then
    begin
      let upname = Tile.get_name (Battlefield.get_tile map up) in
      if texture_name = "water" && is_ground upname then
        draw_v "ground_water_v" up ()
      else if is_ground texture_name && upname = "water" then
        draw_v "water_ground_v" up ()
    end ;
    if self#filter_positions map left then
    begin
      let leftname = Tile.get_name (Battlefield.get_tile map left) in
      if texture_name = "water" && is_ground leftname then
        draw_h "water_ground_hr" left ()
      else if is_ground texture_name && leftname = "water" then
        draw_h "water_ground_h" left ()
    end

  (* Highlight a tile *)
  method private highlight_tile (target : render_window)
    camera base_color pos = 
    let (r,g,b,a) = Color.(
      base_color.r, base_color.g, base_color.b, 
      float_of_int (base_color.a)) in
    let multiplier = sin (Unix.gettimeofday () *. 2.) +. 2. in 
    let alpha = int_of_float ((multiplier /. 3.) *. a) in 
    self#draw_from_map target camera "highlight" pos 
      ~color:(Color.rgba r g b alpha)
      ~blend_mode:BlendAdd ()

  (* Render the whole map (also draws the tile VAO) *)
  method private render_map (target : render_window) 
    camera (map : Battlefield.t) =
    let tileset = TilesetLibrary.get_tileset tileset_library "tileset" in
    let jointset = TilesetLibrary.get_tileset tileset_library "tilejoints" in
    let render_tile tile_name p = 
      self#draw_tile_from_map camera tileset tile_name p ();
      self#render_joints camera jointset p tile_name map
    in
    List.iter
      (fun p -> render_tile (Tile.get_name (Battlefield.get_tile map p)) p)
      (Position.square camera#top_left camera#bottom_right);
    target#draw tileset#vao ~texture:tileset#texture;
    tileset#vao#clear;
    target#draw jointset#vao ~texture:jointset#texture;
    jointset#vao#clear

  (* Render a path with arrows *)
  method private draw_path (target : render_window) 
    camera path =
    let draw = self#draw_from_map target camera in
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

  (* Render a unit *)
  method private draw_unit (target : render_window) 
    camera my_unit =
    self#draw_from_map target camera (my_unit#name) (my_unit#position) ()
 
  (* Render a range (move or attack, according to cursor's state) *)
  method private draw_range (target : render_window) 
    camera map =
    match camera#cursor#get_state with
    |Cursor.Idle -> ()
    |Cursor.Displace(_,_,(range,_)) -> begin
      List.iter (self#highlight_tile target camera 
        (Color.rgba 255 255 100 150)) range
    end
    |Cursor.Action(my_unit, pos) -> begin
      let attack_range = 
        List.filter (self#filter_positions map)
        (Position.range pos 1 my_unit#attack_range)
      in
      List.iter (self#highlight_tile target camera 
        (Color.rgba 255 50 50 255)) attack_range
    end

  (* Draw the cursor *)
  method private draw_cursor (target : render_window) 
    (camera : Camera.camera) =
    self#draw_from_map target camera "cursor" camera#cursor#position 
      ~offset:(Utils.subf2D (0.,0.) camera#cursor#offset) 
      ~scale:(camera#cursor#scale, camera#cursor#scale) ()

  (* Draw the GUI *)
  method draw_gui (target : render_window) 
    (ui_manager : UIManager.ui_manager) =
    ui_manager#draw target texture_library

  (* Draw the whole game *)
  method render_game (target : render_window)
    (data : ClientData.client_data) =
    self#render_map target data#camera data#map;
    self#draw_range target data#camera data#map;
    self#draw_path target data#camera data#current_move;
    self#draw_cursor target data#camera;
    List.iter (fun p -> 
      List.iter (self#draw_unit target data#camera) p#get_army
      ) data#players;
    data#minimap#draw target data#camera#cursor;
    FPS.display target

end

