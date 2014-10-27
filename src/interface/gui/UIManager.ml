open Widget
open OcsfmlGraphics

class ui_manager = object(self)

  val mutable widgets : widget list = []

  method on_event e = 
    let rec event_aux = function
      |[] -> false
      |t::q -> t#on_event e || (event_aux q)
    in event_aux widgets

  method add_widget w = 
    widgets <- w :: widgets

  method draw target texlib = 
    List.iter (fun w -> w#draw target texlib) widgets
end
