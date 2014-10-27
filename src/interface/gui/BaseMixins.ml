open OcsfmlGraphics
open OcsfmlWindow
open Utils
open Widget


class virtual ['a] widget_container = object(self)

  inherit widget as super

  constraint 'a = #widget

  val mutable children : 'a list = []

  method add_child (w : 'a) =
    children <- w::children;
    w#set_parent (Some (self :> widget));
    self#add_event (fun e -> w#on_event e)

  method children = children

  method toggle = 
    super#toggle;
    List.iter (fun c -> c#toggle) children
    

end


class virtual ['a] evq_container = object(self)

  inherit ['a] widget_container as super

  val virtual mutable item_height : int

  method add_child w = 
    super#add_child w;
    size <- (fst size, snd size + item_height);
    w#set_size (fst size, item_height);
    w#set_position (0, (List.length children - 1) * item_height)

  
end
  


class virtual key_ctrl_list = object(self)

  val mutable selected = 0

  val virtual mutable nb_items : int

  method virtual add_event : (Event.t -> bool) -> unit

  method selected = selected

  initializer
    self#add_event (function
      |Event.KeyPressed {Event.code = KeyCode.Up; _} -> 
          nb_items <> 0 
          && (selected <- (selected - 1 + nb_items) mod nb_items; true)
      |Event.KeyPressed {Event.code = KeyCode.Down; _} ->
          nb_items <> 0
          && (selected <- (selected + 1 + nb_items) mod nb_items; true)
      | _ -> false)
end




