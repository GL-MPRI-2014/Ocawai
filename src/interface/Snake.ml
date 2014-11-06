open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val font = new font `None

  val map = Array.make_matrix 16 10 false

  val mutable running = true

  val mutable current_pos = Position.create (0,0)
  val mutable snake = Path.init (Position.create (0,0))
  val mutable last_dir = (1,0)

  val tl = Position.create (0,0)
  val br = Position.create (15,9)

  method private move diff =
    let new_pos = add2D (Position.topair current_pos) diff in
    current_pos <- Position.create new_pos;
    if Position.out_of_bounds current_pos tl br then
      running <- false
    else begin
      let (x,y) = Position.topair current_pos in
      if map.(x).(y) then
        running <- false
      else begin
        map.(x).(y) <- true;
        last_dir <- diff;
        snake <- Path.reach snake current_pos
      end
    end

  (* Inspired by @VLanvin *)
  val mutable last_event = 0.
  method private handle_keys =
    let act_time = Unix.gettimeofday () in
    if act_time -. last_event >= 0.1 then OcsfmlWindow.(
      last_event <- act_time;
      let diff = if Keyboard.is_key_pressed KeyCode.Right then (1,0)
        else if Keyboard.is_key_pressed KeyCode.Left then (-1,0)
        else if Keyboard.is_key_pressed KeyCode.Up then (0,-1)
        else if Keyboard.is_key_pressed KeyCode.Down then (0,1)
        else last_dir
      in
      if add2D diff last_dir = (0,0) then self#move last_dir
      else self#move diff
    )

  method private topos pos =
    let (w,h) = foi2D manager#window#get_size in
    let dx = w /. 2. -. 400. +. 25.
    and dy = h /. 2. -. 250. +. 25. in
    let (x,y) = foi2D (Position.topair pos) in
    (x *. 50. +. dx, y *. 50. +. dy)

  method private draw_path (target : OcsfmlGraphics.render_window) path =
    let draw pos rot name = Render.draw_txr target name (self#topos pos) rot in
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

  method handle_event e =

    OcsfmlWindow.Event.(
      match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } ->
            if not running then begin
              for i = 0 to 15 do
                for j = 0 to 9 do
                  map.(i).(j) <- false
                done
              done;
              snake <- Path.init (Position.create (0,0));
              current_pos <- Position.create (0,0);
              last_dir <- (1,0);
              running <- true
            end
        | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; _ } ->
            manager#pop
        | _ -> ()
    )

  method render window =

    super#render window ;

    let color = Color.rgb 19 42 69 in
    window#clear ~color ();

    let (w,h) = foi2D window#get_size in

    (* Bounding area *)
    new rectangle_shape
      ~size: (800.,500.)
      ~outline_color: Color.red
      ~outline_thickness: 5.
      ~position:(w /. 2., h /. 2.)
      ~origin: (400.,250.)
      ~fill_color: color
      ()
    |> window#draw;

    self#draw_path window (Path.get_move snake);

    if running then
      self#handle_keys
    else begin
      rect_print
        window "GAME OVER" font Color.white (Pix 120) (Pix 10) Center
        { left = 0. ; top = h /. 2. -. 100. ; width = w ; height = 100. };
      rect_print
        window "Press space to continue, q to quit."
        font Color.white (Pix 50) (Pix 10) Center
        { left = 0. ; top = h /. 2. +. 185. ; width = w ; height = 100. };
    end;

    window#display

  initializer
    if not (font#load_from_file "resources/fonts/Roboto-Black.ttf")
    then failwith "Couldn't load the font here";
    ignore (Thread.create Midi_player.play_midi_file "src/music/tetris.mid")

end
