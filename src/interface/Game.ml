open OcsfmlGraphics
open Utils

open Manager

open Player
open Menus

(* Type of selectable *)
type selectable = [
  | `Unit of Unit.t
  | `Building of Building.t
]

let new_game ?character () =

  let m_cdata = new ClientData.client_data in

  let my_player = new ClientPlayer.client_player m_cdata#push_update in

  let m_engine = new Game_engine.game_engine () in

  let (m_players, m_map) = m_engine#init_local (my_player :> player) 3 in

  let m_camera = new Camera.camera
    ~def_tile_size:50
    ~w:manager#window#get_width ~h:manager#window#get_height
    ~initpos:(m_engine#cursor_init_position (my_player :> player)#get_id)
    ~maxpos:(Position.diff
      (Position.create (Battlefield.size m_map))
      (Position.create (1,1)))
  in

  let m_uphandle = new Updates.handler m_cdata m_camera in

  (* Distributing characters *)
  let () =
    let constraints = match character with
      | Some c -> [((my_player :> player)#get_id,c)]
      | None -> []
    in
    Characters.handler#init
      constraints
      (List.map (fun p -> p#get_id) m_engine#get_players)
  in

  object(self)

  inherit State.state as super

  val ui_manager = new UIManager.ui_manager

  val camera = m_camera

  val cdata : ClientData.client_data = m_cdata

  val uphandle = m_uphandle

  val disp_menu = new ingame_menu ~m_position:(0,0) ~m_width:150
    ~m_item_height:30 ~m_theme:Theme.yellow_theme
    ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Action"

  val atk_menu = new ingame_menu ~m_position:(0,0) ~m_width:150
    ~m_item_height:30 ~m_theme:Theme.red_theme
    ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Attack"

  val build_menu = new ingame_menu ~m_position:(0,0)
    ~m_width:290
    ~m_item_height:30 ~m_theme:Theme.yellow_theme
    ~m_bar_height:30 ~m_bar_icon:"menu_icon"
    ~m_bar_text:"Build"

  (* Last unit selected through X/W *)
  val mutable last_selected : selectable option = None

  (* Select next unit given a way to go *)
  method private select_playable lu lb =
    let rec list_after x = function
      | [] -> []
      | e :: r when x = e -> r
      | e :: r -> list_after x r
    in
    (* Selectable unit *)
    let oku u = not u#has_played in
    (* Selectable building *)
    let okb b =
      b#product <> []
      && cdata#player_unit_at_position
          b#position
          cdata#actual_player
         = None
    in
    let find_u lu fail () =
      try last_selected <- Some ( `Unit (List.find oku lu) )
      with Not_found -> fail ()
    in
    let find_b lb fail () =
      try last_selected <- Some ( `Building (List.find okb lb) )
      with Not_found -> fail ()
    in
    let fail () =
      last_selected <- None
    in
    begin
      match last_selected with
      | None -> find_u lu (find_b lb fail)
      | Some (`Unit u) -> find_u (list_after u lu) (find_b lb (find_u lu fail))
      | Some (`Building b) ->
          find_b (list_after b lb) (find_u lu (find_b lb fail))
    end () ;
    begin match last_selected with
    | None -> ()
    | Some (`Unit u) -> cdata#camera#set_position u#position
    | Some (`Building b) -> cdata#camera#set_position b#position
    end

  (* Select next playable unit or building *)
  method private select_next =
    (* List of units *)
    let lu = cdata#actual_player#get_army
    (* List of buildings *)
    and lb = cdata#actual_player#get_buildings in
    self#select_playable lu lb

  (* Select previous playable unit or building *)
  method private select_pred =
    (* List of units *)
    let lu = List.rev cdata#actual_player#get_army
    (* List of buildings *)
    and lb = List.rev cdata#actual_player#get_buildings in
    self#select_playable lu lb

  initializer
    cdata#init_core m_map my_player m_players;
    cdata#init_buildings m_engine#get_neutral_buildings;
    cdata#init_interface m_camera

  method private create_ui =
    (* Main ingame menu *)
    let my_menu = new ingame_menu
      ~m_position:(manager#window#get_width / 2 - 75, 30) ~m_width:150
      ~m_item_height:30 ~m_theme:Theme.blue_theme
      ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Menu" in

    (* Forfeit confirmation popup *)
    let forfeit_popup = new Windows.ingame_popup
      ~m_position:(manager#window#get_width / 2 - 200,
        manager#window#get_height / 2 - 80)
      ~m_size:(400, 110) ~m_theme:Theme.blue_theme
      ~m_text:("Do you really want to forfeit ? The game will be considered "
                ^ "lost... Also, notice how this text is perfectly handled ! "
                ^ "This is beautiful isn't it ?")
      ~m_bar_height:30 ~m_bar_icon:"menu_icon" ~m_bar_text:"Forfeit" in

    (* Buttons for the forfeit popup *)
    new Windows.text_framed_item
      (50, 70) (100, 25) "Yes !" (fun () -> Manager.manager#pop)
      Theme.blue_theme
    |> forfeit_popup#add_child;

    new Windows.text_framed_item
      (250, 70) (100, 25) "No !" (fun () -> ui_manager#unfocus forfeit_popup;
        forfeit_popup#toggle) Theme.blue_theme
    |> forfeit_popup#add_child;

    (* Button to open ingame menu *)
    let main_button = new key_button_oneuse ~icon:"return"
      ~text:"Menu" ~m_size:(150, 30) ~keycode:(OcsfmlWindow.KeyCode.Return)
      ~m_position:(manager#window#get_width / 2 - 75, 0)
      ~callback:(fun () -> my_menu#toggle; ui_manager#focus my_menu)
      ~m_theme:Theme.blue_theme
    in

    (* Ingame menu items *)
    new item "cancel" "End turn" (fun () ->
      if cdata#actual_player#event_state = ClientPlayer.Waiting then
        cdata#actual_player#set_state (ClientPlayer.Received ([], Action.End_turn));
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    new item "forfeit" "Forfeit" (fun () -> forfeit_popup#toggle;
      ui_manager#focus forfeit_popup; my_menu#toggle; main_button#toggle)
    |> my_menu#add_child;

    (* new item "info" "Info" (fun () -> print_endline "info activated";
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child; *)

    new item "params" "Settings" (fun () -> new SettingsScreen.state |> manager#push ;
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    new item "cancel" "Cancel" (fun () -> print_endline "canceled";
      my_menu#toggle; main_button#toggle; ui_manager#unfocus my_menu)
    |> my_menu#add_child;

    let cursor = cdata#camera#cursor in

    (* Attack menu items *)
    new item "fire" "Fire !" (fun () ->
      atk_menu#toggle;
      ui_manager#unfocus atk_menu;
      let cursor = cdata#camera#cursor in
      let atking_unit =
        match cursor#get_state with
        |Cursor.Action(u,_,_) -> u
        | _ -> assert false
      in
      let atked_unit  =
        match cdata#unit_at_position cursor#position with
        |Some(u) -> u
        |None -> assert false
      in
      cdata#actual_player#set_state (ClientPlayer.Received
        (cdata#current_move, Action.Attack_unit (atking_unit, atked_unit)));
      cursor#set_state Cursor.Idle)
    |> atk_menu#add_child;

    new item "cancel" "Cancel" (fun () ->
      atk_menu#toggle;
      ui_manager#unfocus atk_menu;
      cursor#set_state Cursor.Idle)
    |> atk_menu#add_child;

    (* Displacement menu items *)
    new item "attack" "Attack" (fun () ->
      disp_menu#toggle;
      ui_manager#unfocus disp_menu;
      match cursor#get_state with
      |Cursor.Displace(_,u,(r,_)) ->
        let in_range = Logics.units_inrange cursor#position
               (u#min_attack_range, u#attack_range)
               (cdata#actual_player :> Player.logicPlayer)
               cdata#players
        in
        if List.mem cursor#position r && in_range <> [] then begin
          cursor#set_state (Cursor.Action
            (u, cursor#position, in_range));
          camera#set_position (List.hd in_range)#position
        end else if List.mem cursor#position r then begin
          cdata#actual_player#set_state (ClientPlayer.Received
            (cdata#current_move, Action.Wait));
          cursor#set_state Cursor.Idle
        end else
          cursor#set_state Cursor.Idle
      | _ -> assert false)
    |> disp_menu#add_child;

    new item "move" "Move" (fun () ->
      disp_menu#toggle;
      ui_manager#unfocus disp_menu;
      cdata#actual_player#set_state (ClientPlayer.Received
        (cdata#current_move, Action.Wait));
      cursor#set_state Cursor.Idle)
    |> disp_menu#add_child;

    new item "cancel" "Cancel" (fun () ->
      disp_menu#toggle;
      ui_manager#unfocus disp_menu;
      cursor#set_state Cursor.Idle)
    |> disp_menu#add_child;

    (* Build menu items *)

    new item "cancel" "Cancel" (fun () ->
      build_menu#toggle;
      ui_manager#unfocus build_menu;
      cursor#set_state Cursor.Idle)
    |> build_menu#add_child;

    my_menu#toggle;
    disp_menu#toggle;
    atk_menu#toggle;
    build_menu#toggle;

    forfeit_popup#toggle;

    ui_manager#add_widget forfeit_popup;
    ui_manager#add_widget main_button;
    ui_manager#add_widget my_menu;
    ui_manager#add_widget disp_menu;
    ui_manager#add_widget atk_menu;
    ui_manager#add_widget build_menu

  initializer
    self#create_ui;
    Thread.create (fun () -> m_engine#run) ()
    |> ignore

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
    if not (ui_manager#on_event e) then OcsfmlWindow.Event.(
      begin match e with
        | KeyPressed { code = OcsfmlWindow.KeyCode.T ; _ } ->
            (* TODO Remove? *)
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

        | KeyPressed { code = OcsfmlWindow.KeyCode.X ; _ } ->
            self#select_next

        | KeyPressed { code = OcsfmlWindow.KeyCode.W ; _ } ->
            self#select_pred

        | KeyPressed { code = OcsfmlWindow.KeyCode.Space ; _ } when
            cdata#actual_player#event_state = ClientPlayer.Waiting -> Cursor.(
              let cursor = cdata#camera#cursor in
              match cursor#get_state with
              |Idle -> begin
                match cdata#player_unit_at_position cursor#position
                      cdata#actual_player with
                | Some u when (not u#has_played) ->
                    cursor#set_state (Displace (cdata#map, u,
                      Logics.accessible_positions u
                     (cdata#actual_player :> logicPlayer)
                      cdata#players
                      cdata#map))
                | None ->
                    (* We only check out buildings where there are no unit *)
                    begin match cdata#building_at_position cursor#position with
                      | (Some b, Some p) when
                        p = (cdata#actual_player :> logicPlayer) ->
                          if b#product <> [] then begin
                            (* Compute the list of producibles into a menu *)
                            build_menu#clear_children;
                            let item = new item "cancel" "Cancel" (fun () ->
                              build_menu#toggle;
                              ui_manager#unfocus build_menu;
                              cursor#set_state Cursor.Idle)
                            in item#toggle ; build_menu#add_child item;
                            List.iter (fun s ->
                              let u = List.find
                                (fun u -> u#name = s)
                                Config.config#unbound_units_list
                              in
                              new item
                                ~enabled:(u#price <= p#get_value_resource)
                                (Characters.handler#texture_from_id (p#get_id) s)
                                (s ^ " (" ^ (string_of_int u#price) ^ ")")
                                (fun () ->
                                  build_menu#toggle;
                                  ui_manager#unfocus build_menu;
                                  cdata#actual_player#set_state (
                                    ClientPlayer.Received ([],
                                      Action.Create_unit (b,u)
                                    )
                                  ) ;
                                  cursor#set_state Cursor.Idle
                                )
                              |> (fun i -> i#toggle ; build_menu#add_child i)
                            ) b#product;
                            build_menu#toggle;
                            build_menu#set_position
                              (cdata#camera#project cursor#position) ;
                            ui_manager#focus build_menu ;
                            cursor#set_state (Build b)
                          end
                      | _ -> ()
                    end
                | _ -> ()
              end
              | Displace (_,u,(acc,_)) ->
                  let uopt = cdata#unit_at_position cursor#position in
                  begin match uopt with
                  |None when List.mem cursor#position acc ->
                      disp_menu#set_position
                        (cdata#camera#project cursor#position);
                      ui_manager#focus disp_menu;
                      disp_menu#toggle
                  |Some (u') when u = u' && List.mem cursor#position acc ->
                      disp_menu#set_position
                        (cdata#camera#project cursor#position);
                      ui_manager#focus disp_menu;
                      disp_menu#toggle
                  |_ ->
                      cursor#set_state Idle
                  end
              | Action(_,_,_) ->
                  atk_menu#toggle;
                  atk_menu#set_position (cdata#camera#project cursor#position);
                  ui_manager#focus atk_menu
              | Build b -> ())

        | KeyPressed { code = OcsfmlWindow.KeyCode.Escape ; _ } ->
            cdata#camera#cursor#set_state Cursor.Idle
        | _ -> ()
      end)

  method render window =
    self#keyboard_events;
    Interpolators.update () ;
    window#clear ();

    cdata#minimap#compute cdata#map cdata#players;

    (* Rendering goes here *)
    Render.renderer#render_game window cdata uphandle;
    Render.renderer#draw_gui window ui_manager;

    window#display

end
