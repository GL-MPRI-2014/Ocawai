open OcsfmlGraphics
open Utils

open Manager

open Player
open Menus

class game = object(self)

  inherit State.state as super

  val generator = new FieldGenerator.t 100 100 2 10 5

  val ui_manager = new UIManager.ui_manager

  val camera = new Camera.camera
    ~def_tile_size:50
    ~w:manager#window#get_width ~h:manager#window#get_height
    ~maxpos:(Position.create (99,99))

  (*val cdata = new ClientData.client_data ~camera
    ~map:(generator#field)
    ~players:(List.map (fun a -> 
      let p = Player.create_player () in 
      p#set_army a) generator#armies)*)

  val mutable cdata : ClientData.client_data option = None

  method private create_ui =
    let my_menu = new menu (manager#window#get_width / 2 - 75, 30) 150 30
    OcsfmlWindow.KeyCode.Return Theme.blue_theme 30 "menu_icon" "Menu" in

    let main_button = new key_button_oneuse ~icon:"return"
      ~text:"Menu" ~m_size:(150, 30) ~keycode:(OcsfmlWindow.KeyCode.Return)
      ~m_position:(manager#window#get_width / 2 - 75, 0)
      ~callback:(fun () -> my_menu#toggle; ui_manager#focus my_menu) ~m_theme:Theme.blue_theme
    in

    new item "forfeit" "Forfeit" (fun () -> print_endline "forfeited"; Manager.manager#pop)
    |> my_menu#add_child;

    new item "info" "Info" (fun () -> print_endline "info activated";
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    new item "params" "Settings" (fun () -> print_endline "settings activated";
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    new item "infantry" "Cancel" (fun () -> print_endline "canceled";
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    ui_manager#add_widget main_button;
    my_menu#toggle;
    ui_manager#add_widget my_menu

  initializer
    self#create_ui;
    cdata <-Some (new ClientData.client_data ~camera
      ~map:(generator#field)
      ~players:(List.map (fun a -> 
        let p = Player.create_player () in 
        p#set_army a; p) generator#armies))

  val mutable last_event = 0.
  val mutable dir_key_pressed = false

  method private keyboard_events = 
    let act_time = Unix.gettimeofday () in 
    if (not ui_manager#is_focusing) && 
     act_time -. last_event >= 0.05 then OcsfmlWindow.(
      last_event <- act_time;
      if Keyboard.is_key_pressed KeyCode.Right ||
         Keyboard.is_key_pressed KeyCode.Left  ||
         Keyboard.is_key_pressed KeyCode.Up    ||
         Keyboard.is_key_pressed KeyCode.Down  then
          dir_key_pressed <- true
      else
          dir_key_pressed <- false;
      if Keyboard.is_key_pressed KeyCode.Right then
        camera#move (1,0);
      if Keyboard.is_key_pressed KeyCode.Left then
        camera#move (-1,0);
      if Keyboard.is_key_pressed KeyCode.Up then
        camera#move (0,-1);
      if Keyboard.is_key_pressed KeyCode.Down then
        camera#move (0,1);
      if Keyboard.is_key_pressed KeyCode.Z then
        camera#set_zoom (camera#zoom *. 1.1);
      if Keyboard.is_key_pressed KeyCode.A then
        camera#set_zoom (camera#zoom *. 0.90909)
    )

  method handle_event e =
    (* Ugly *)
    let cdata = match cdata with
      | Some c -> c
      | None -> failwith "Oh no !\n"
    in

    if not (ui_manager#on_event e) then OcsfmlWindow.Event.(
      begin match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.T ; _ } ->
            camera#set_position (Position.create (80,80))

        | KeyPressed { code = OcsfmlWindow.KeyCode.Left; _ } ->
            if not dir_key_pressed then begin
              camera#move (-1,0);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { code = OcsfmlWindow.KeyCode.Up; _ } ->
            if not dir_key_pressed then begin
              camera#move (0,-1);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { code = OcsfmlWindow.KeyCode.Right; _ } ->
            if not dir_key_pressed then begin
              camera#move (1,0);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { code = OcsfmlWindow.KeyCode.Down; _ } ->
            if not dir_key_pressed then begin
              camera#move (0,1);
              last_event <- Unix.gettimeofday() +. 0.2
            end

        | KeyPressed { code = OcsfmlWindow.KeyCode.Num0 ; _ } ->
            camera#set_zoom 1.

        | KeyPressed { code = OcsfmlWindow.KeyCode.M ; _ } ->
            camera#toggle_zoom

        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } -> Cursor.(
              let cursor = cdata#camera#cursor in
              match cursor#get_state with
              |Idle -> cdata#unit_at_position cursor#position >?
                (fun u -> cursor#set_state (Displace (cdata#map, u, 
                  Logics.accessible_positions u 
                    (cdata#player_of u)
                     cdata#players
                     cdata#map))
                )
              |Displace(_,u,(r,_)) -> 
                  if List.mem cursor#position r then 
                    cursor#set_state (Action (u,cursor#position))
                  else
                    cursor#set_state Idle
              |Action(p,u) -> cursor#set_state Idle)
        | _ -> ()
      end)

  method render window =
    self#keyboard_events;
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
