class virtual state = object(self)

  method render (window : OcsfmlGraphics.render_window) = ()

  method handle_event (e : OcsfmlWindow.Event.t) = ()

  method destroy = ()

end
