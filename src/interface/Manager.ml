let manager = object(self)

  val mutable states : State.state list = []
  val mutable window : OcsfmlGraphics.render_window =
    new OcsfmlGraphics.render_window
      (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
      "Flower Wars"
      ~style: [OcsfmlWindow.Window.Fullscreen]

  initializer
    Render.load_ressources ();
    window#set_key_repeat_enabled false

  method window : OcsfmlGraphics.render_window = window

  method push (state : State.state) =
    states <- state :: states

  method push_load (state : State.state) (build : unit -> State.state) =
    self#push state ;
    let _ =
      Thread.create
        (fun () ->
          try let s = build () in self#pop ; self#push s
          with e -> self#pop ; raise e) ()
    in ()


  method pop =
    self#current#destroy;
    states <- List.tl states

  method current = List.hd states

  method is_running = states <> []

  method event_loop =
    match window#poll_event with
    | Some e ->
        OcsfmlWindow.Event.(
        begin match e with
          | Closed
          | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; control = true ; _ }
          | KeyPressed { code = OcsfmlWindow.KeyCode.C ; control = true ; _ } ->
              window#close

          | KeyPressed { code = OcsfmlWindow.KeyCode.Escape ; _ } ->
              window#close;
              window#create
                (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
                "Flower Wars";
              window#set_key_repeat_enabled false

          | KeyPressed { code = OcsfmlWindow.KeyCode.F ; _ } ->
              window#close;
              window#create
                ~style: [OcsfmlWindow.Window.Fullscreen]
                (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
                "Flower Wars";
              window#set_key_repeat_enabled false

          | Resized _ ->

              ()

          | _ -> self#current#handle_event e
        end) ;
        self#event_loop
    | None -> ()

  method run =

    if not window#is_open then states <- [] ;

    if self#is_running then
    begin
      self#event_loop ;
      self#current#render window ;
      self#run
    end

end
