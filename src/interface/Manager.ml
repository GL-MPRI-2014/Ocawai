let manager = object(self)

  val mutable states : State.state list = []
  val mutable fullscreen = true
  val mutable window : OcsfmlGraphics.render_window =
    if Array.length (OcsfmlWindow.VideoMode.get_full_screen_modes ()) = 0
    then new OcsfmlGraphics.render_window
              (OcsfmlWindow.VideoMode.create ~w:800 ~h:600 ())
              "OCAWAI"
    else new OcsfmlGraphics.render_window
              (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
              ~style:[OcsfmlWindow.Window.Fullscreen]
              "OCAWAI"

  initializer
    Render.renderer#init;
    Sounds.load_sounds ();
    window#set_key_repeat_enabled true;
    window#set_framerate_limit 60

  method window : OcsfmlGraphics.render_window = window

  method reset_window =
    window#close ;
    if fullscreen then
      window#create
      ~style: [OcsfmlWindow.Window.Fullscreen]
      (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
      "OCAWAI"
    else
      window#create
      (OcsfmlWindow.VideoMode.create ~w:800 ~h:600 ())
      "OCAWAI" ;
    window#set_key_repeat_enabled true ;
    window#set_framerate_limit 60

  method set_fullscreen b =
    fullscreen <- b ;
    self#reset_window

  method push (state : State.state) =
    if self#is_running then self#current#paused ;
    states <- state :: states


  method pop =
    self#current#destroy ;
    states <- List.tl states ;
    if self#is_running then self#current#resumed

  method current = List.hd states

  method is_running = states <> []

  method event_loop =
    if self#is_running then
    match window#poll_event with
    | Some e ->
        OcsfmlWindow.Event.(
        begin match e with
          | Closed
          | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; control = true ; _ }
          | KeyPressed { code = OcsfmlWindow.KeyCode.C ; control = true ; _ } ->
              window#close

          | Resized _ -> ()

          | _ -> self#current#handle_event e
        end) ;
        self#event_loop
    | None -> ()

  method run =

    if not window#is_open then while self#is_running do self#pop done ;

    if self#is_running then self#event_loop ;
    if self#is_running then
    begin
      self#current#render window ;
      self#run
    end

end
