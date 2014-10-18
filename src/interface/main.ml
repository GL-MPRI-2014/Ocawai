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
    ~w:window#get_width ~h:window#get_height in

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
              
        | _ -> ()
      end);
      event_loop ()
    | None  -> ()
  in

  let rec main_loop () =
    if window#is_open then begin
      event_loop ();
      window#clear ();
      (* Rendering goes here *)
      (* For testing purpose we will draw a Map right there *)
      Render.render_map window camera (Battlefield.dummy_map ());
      (* end of test *)
      window#display;
      main_loop ()
    end
  in

  main_loop ()
end
