(** Main function for the interface *)
open OcsfmlGraphics
open Utils

(* Needed to compile them (otherwise no doc) *)
open Player
open Menus

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

  (* Can be dimensioned as we like *)
  (* Here, it will be 120 pixels large, and 30 pixels tall per item *)
  let my_menu = new menu (300,0) 120 30 in

  new item "infantry" "item 1" (fun () -> print_endline "item 1 activated")
  |> my_menu#add_child;

  new item "infantry" "item 2" (fun () -> print_endline "item 2 activated")
  |> my_menu#add_child;

  new item "infantry" "item 3" (fun () -> print_endline "item 3 activated")
  |> my_menu#add_child;

  new item "infantry" "item 4" (fun () -> print_endline "item 4 activated")
  |> my_menu#add_child;

  (* Basic event manipulation *)
  let rec event_loop () =
    match window#poll_event with
    | Some e -> OcsfmlWindow.Event.(
      (* just a test *)
      my_menu#on_event e;
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

      (* This is really garbage *)
      Render.render_widget window my_menu;

      (* end of test *)
      window#display;
      main_loop ()
    end
  in

  main_loop ()
end
