class virtual state = object(self)

  method render (window : OcsfmlGraphics.render_window) =
    ()
    (* Not possible -- circular build *)
    (* if not window#is_open then
      Manager.manager#pop *)

  method handle_event (e : OcsfmlWindow.Event.t) = ()

end
