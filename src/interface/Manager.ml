let manager = object(self)

  val mutable states : State.state list = []
  val window : OcsfmlGraphics.render_window = new OcsfmlGraphics.render_window
    (OcsfmlWindow.VideoMode.get_full_screen_modes ()).(0)
    "Flower Wars"
    ~style: [OcsfmlWindow.Window.Fullscreen]

  method push (state : State.state) =
    states <- state :: states

  method pop =
    states <- List.tl states

  method current = List.hd states

  method is_running = states <> []

  method event_loop =
    match window#poll_event with
    | Some e ->
        self#current#handle_event e ;
        self#event_loop
    | None -> ()

  method run =

    Printf.printf "Hello ?\n";
    if self#is_running then
    begin
      self#event_loop ;
      self#current#render window ;
      self#run
    end

end
