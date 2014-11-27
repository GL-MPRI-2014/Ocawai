class virtual state = object(self)

  method virtual render : OcsfmlGraphics.render_window -> unit

  method handle_event (e : OcsfmlWindow.Event.t) = ()

  method destroy = ()

end
