(** Main function for the interface *)
open OcsfmlGraphics
open Utils

(* Needed to compile them (otherwise no doc) *)
open Player
open Menus

let create_ui manager = 
  (* Can be dimensioned as we like *)
  (* Here, it will be 120 pixels large, and 30 pixels tall per item *)
  let my_menu = new menu (300,50) 150 30 in

  new item "forfeit" "Forfeit" (fun () -> print_endline "forfeited")
  |> my_menu#add_child;

  new item "info" "Info" (fun () -> print_endline "info activated")
  |> my_menu#add_child;

  new item "params" "Settings" (fun () -> print_endline "settings activated")
  |> my_menu#add_child;

  new item "infantry" "item 4" (fun () -> print_endline "item 4 activated")
  |> my_menu#add_child;

  manager#add_widget (my_menu :> Widget.widget);
  (* Return it because we need it in the main (until we find a better solution)
   *)
  my_menu


let () = begin

  let generator = new FieldGenerator.t 100 100 1 in (* map : 100*100, 1 player *)
  print_endline "generation ok";
  
  let window = new render_window
    (* (OcsfmlWindow.VideoMode.create ~w:800 ~h:600 ()) *)
    (* (OcsfmlWindow.VideoMode.get_desktop_mode ()) *)
    (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
    "Flower Wars"
    ~style: [OcsfmlWindow.Window.Fullscreen]
  in

  let ui_manager = new UIManager.ui_manager in

  let my_menu = create_ui ui_manager in

  Render.load_ressources () ;

  (* Main window *)

  let camera = new Camera.camera ~def_tile_size:50
    ~w:window#get_width ~h:window#get_height
    ~maxpos:(Position.create (99,99)) in

  let num = 0 in (*player 1*)
  let cdata = new ClientData.client_data ~camera
    ~map:(generator#field)
    ~units:(List.nth (generator#armies) num) in

  (* Basic event manipulation *)
  let rec event_loop () =
    match window#poll_event with
    | Some e when ui_manager#on_event e -> event_loop ()
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

        | KeyPressed { code = OcsfmlWindow.KeyCode.Z ; _ } ->
            camera#set_zoom (camera#zoom *. 1.1)

        | KeyPressed { code = OcsfmlWindow.KeyCode.A ; _ } ->
            camera#set_zoom (camera#zoom *. 0.90909)

        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } ->
            begin
              match cdata#selected with
              | Some u ->
                  cdata#unselect;
                  cdata#camera#cursor#stop_moving
              | None ->
                  begin match cdata#unit_at_position
                    cdata#camera#cursor#position with
                    | Some u ->
                        cdata#select_unit u;
                        cdata#camera#cursor#set_moving
                    | None ->
                        my_menu#toggle;
                        my_menu#set_position (camera#project cdata#camera#cursor#position)
                  end
            end

        | Resized _ ->
          (* We have to do something here *)
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
