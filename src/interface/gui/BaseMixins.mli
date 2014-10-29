open OcsfmlWindow

(** A mixin that represents widget containers *)
class virtual ['a] widget_container : object

  inherit Widget.widget

  constraint 'a = #Widget.widget

  method add_child : 'a -> unit

  method children : 'a list

end


(** A mixin that represents an Expandable Vertical container, where all
  * item heights are equal *)

class virtual ['a] evq_container : object

  inherit ['a] widget_container

  val virtual mutable item_height : int

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

end


(** A mixin that represents a keyboard-controlled list (up/down) *)
class virtual key_ctrl_list : object

  val virtual mutable nb_items : int

  method virtual add_event : (Event.t -> bool) -> unit

  method selected : int

end
