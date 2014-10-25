(** Main function for the interface *)
open OcsfmlGraphics
open Utils

(* Needed to compile them (otherwise no doc) *)
open Player

let () = begin

  let generator = new FieldGenerator.t 100 100 1 in (* map : 100*100, 1 player *)
  
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

  let num = 0 in (*player 1*)
  let cdata = new ClientData.client_data ~camera
    ~map:(generator#field)
    ~units:(List.nth (generator#armies) num) in

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

        | KeyPressed { code = OcsfmlWindow.KeyCode.Right ; _ } ->
            camera#move (1,0)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Down ; _ } ->
            camera#move (0,1)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Left ; _ } ->
            camera#move (-1,0)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Up ; _ } ->
            camera#move (0,-1)

        | KeyPressed { code = OcsfmlWindow.KeyCode.T ; _ } ->
            camera#set_position (Position.create (80,80))

        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } ->
            begin
              match cdata#selected with
              | Some u ->
                  cdata#unselect;
                  cdata#camera#cursor#stop_moving
              | None ->
                  cdata#unit_at_position cdata#camera#cursor#position
                  >? (fun u -> cdata#select_unit u;
                               cdata#camera#cursor#set_moving)
            end

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
      Interpolators.update ();

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
