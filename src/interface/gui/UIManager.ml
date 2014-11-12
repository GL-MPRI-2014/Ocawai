open Widget
open OcsfmlGraphics

class ui_manager = object(self)

  val mutable widgets : widget list = []

  val mutable focus : widget option = None

  method on_event e = 
    let rec event_aux = function
      |[] -> false
      |t::q -> t#on_event e || (event_aux q)
    in 
    match focus with
    |None -> event_aux widgets
    |Some(w) -> w#on_event e || true

  method focus : 'a. (#Widget.widget as 'a) -> unit = 
    fun w -> focus <- Some (w :> Widget.widget)

  method unfocus : 'a. (#Widget.widget as 'a)  -> unit =
    fun w -> 
      match focus with
      |Some(w') when w' = (w :> Widget.widget) -> focus <- None
      | _ -> ()

  method is_focusing = focus <> None

  method add_widget : 'a. (#Widget.widget as 'a) -> unit = 
    fun w -> widgets <- (w :> Widget.widget) :: widgets

  method draw target texlib = 
    List.iter (fun w -> w#draw target texlib) (List.rev widgets)
end
