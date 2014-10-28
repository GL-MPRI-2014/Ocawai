open OcsfmlGraphics
open Utils

open Manager

open Player
open Menus

class game = object(self)

  inherit State.state as super

  val generator = new FieldGenerator.t 100 100 1

  val ui_manager = new UIManager.ui_manager

  (* How ugly *)
  val mutable my_menu = new menu (0,0) 0 0

  val camera = new Camera.camera
    ~def_tile_size:50
    ~w:manager#window#get_width ~h:manager#window#get_height
    ~maxpos:(Position.create (99,99))

  val num = 0

  (* val cdata = new ClientData.client_data ~camera
    ~map:(generator#field)
    ~units:(List.nth (generator#armies) num) *)

  val mutable cdata : ClientData.client_data option = None

  method create_ui manager =
    (* Can be dimensioned as we like *)
    (* Here, it will be 120 pixels large, and 30 pixels tall per item *)
    let my_menu = new menu (300,50) 150 30 in

    new item "forfeit" "Forfeit" (fun () -> print_endline "forfeited" ; Manager.manager#pop)
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

  initializer

    my_menu <- self#create_ui ui_manager;
    cdata <-Some (new ClientData.client_data ~camera
      ~map:(generator#field)
      ~units:(List.nth (generator#armies) num))

  method handle_event e =
    (* Ugly *)
    let cdata = match cdata with
      | Some c -> c
      | None -> failwith "Oh no !\n"
    in

    if not (ui_manager#on_event e) then OcsfmlWindow.Event.(
      begin match e with
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

          | _ -> ()
      end)

  method render window =

    super#render window ;
    Interpolators.update () ;
    window#clear ();

    (* Ugly *)
    let cdata = match cdata with
      | Some c -> c
      | None -> failwith "Oh no !\n"
    in

    (* Rendering goes here *)
    Render.render_game window cdata;
    Render.draw_hud window;
    Render.draw_gui window ui_manager;

    window#display

end
