open OcsfmlWindow

(** A mixin that represents widget containers *)
class virtual widget_container : object

  inherit Widget.widget

  method add_child : Widget.widget -> unit

  method children : Widget.widget list

end


(** A mixin that represents an Expandable Vertical container, where all
  * item heights are eQual *)

class virtual evq_container : object

  inherit widget_container 

  val virtual mutable item_height : int

end


(** A mixin that represents a keyboard-controlled list (up/down) *)
class virtual key_ctrl_list : object

  val virtual mutable nb_items : int

  method virtual add_event : (Event.t -> unit) -> unit

  method selected : int

end


