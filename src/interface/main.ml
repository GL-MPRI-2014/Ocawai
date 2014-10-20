open OcsfmlGraphics

let () = begin
  (* Main window *)
  let window = new render_window
    (* (OcsfmlWindow.VideoMode.create ~w:800 ~h:600 ()) *)
    (* (OcsfmlWindow.VideoMode.get_desktop_mode ()) *)
    (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
    "Flower Wars"
    ~style: [OcsfmlWindow.Window.Fullscreen]
  in

  let camera = new Camera.camera ~tile_size:50
    ~w:window#get_width ~h:window#get_height
    ~maxpos:(Position.create (99,99)) in

  let cdata = new ClientData.client_data ~camera
    ~map:(Battlefield.dummy_map ())
    ~units:[
      Unit.create_from_file "41" "42";
      Unit.create_from_file "41" "39";
      Unit.create_from_file "39" "39"
    ] in

  (* cdata#set_current_move [
        Position.create (41,42) ;
        Position.create (41,43) ;
        Position.create (42,43) ;
        Position.create (43,43) ;
        Position.create (44,43) ;
        Position.create (45,43) ;
        Position.create (45,42) ;
        Position.create (45,41) ;
        Position.create (44,41) ;
        Position.create (43,41) ;
        Position.create (42,41) ;
        Position.create (41,41) ;
        Position.create (40,41) ;
        Position.create (39,41)
  ]; *)

  cdata#set_current_move [Position.create (41,42)];

  cdata#select_unit (List.hd cdata#units);

  (* We should move that to a dedicated module, or implement State
   * We should also parametrize that with a dt to stabilize camera
   * speed *)
  let check_keys () = OcsfmlWindow.(
    if Keyboard.is_key_pressed KeyCode.Right then
      camera#move (1.5,0.);
    if Keyboard.is_key_pressed KeyCode.Down then
      camera#move (0.,1.5);
    if Keyboard.is_key_pressed KeyCode.Left then
      camera#move (-1.5,0.);
    if Keyboard.is_key_pressed KeyCode.Up then
      camera#move (0.,-1.5))
  in

  (* Basic event manipulation *)
  let rec event_loop () =
    match window#poll_event with
    | Some e -> OcsfmlWindow.Event.(
      begin match e with
        | Closed
        | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; control = true ; _ }
        | KeyPressed { code = OcsfmlWindow.KeyCode.C ; control = true ; _ } ->
          window#close

        | KeyPressed { code = OcsfmlWindow.KeyCode.Escape ; _ } ->
            window#create
              (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
              "Flower Wars"

        | KeyPressed { code = OcsfmlWindow.KeyCode.F ; _ } ->
            window#create
              ~style: [OcsfmlWindow.Window.Fullscreen]
              (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
              "Flower Wars"

        | Resized _ ->
          (* We have to do something here -- or forbid resizing *)
          ()

        | _ -> ()
      end);
      event_loop ()
    | None  -> ()
  in

  let rec main_loop () =
    if window#is_open then begin
      check_keys ();
      event_loop ();
      window#clear ();
      (* Rendering goes here *)

      Render.render_game window cdata;

      Render.draw_hud window;

      (* end of test *)
      window#display;
      main_loop ()
    end
  in

  main_loop ()
end
