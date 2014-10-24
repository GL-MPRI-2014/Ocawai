open OcsfmlGraphics
open OcsfmlWindow
open Utils
open Widget


class virtual widget_container = object(self)

  inherit widget

  val mutable children : widget list = []

  method add_child w =
    children <- w::children;
    w#set_parent (Some (self :> widget));
    self#add_event (fun e -> w#on_event e)

  method children = children

end


class virtual evq_container = object(self)

  inherit widget_container as super

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

  method virtual add_event : (Event.t -> unit) -> unit

  method selected = selected

  initializer
    self#add_event (function
      |Event.KeyPressed {Event.code = KeyCode.Up; _} -> 
          if nb_items <> 0 then 
            selected <- (selected - 1 + nb_items) mod nb_items
      |Event.KeyPressed {Event.code = KeyCode.Down; _} ->
          if nb_items <> 0 then 
            selected <- (selected + 1 + nb_items) mod nb_items
      | _ -> ())
end



