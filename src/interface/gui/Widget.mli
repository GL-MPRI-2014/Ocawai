(**
  * Provides a basic mixin for widgets
  *
  *)

(** Base mixin for all widgets *)
class virtual widget : object

  (** Position of this widget, relative to its parent *)
  val virtual mutable position : int * int

  (** Size of this widget *)
  val virtual mutable size : int * int


  (** Returns the absolute position of this widget *)
  method position : int * int

  (** Adds an event catcher to this widget *)
  method add_event : (OcsfmlWindow.Event.t -> unit) -> unit

  (** Catches the given event *)
  method on_event : OcsfmlWindow.Event.t -> unit

  (** Draw the widget to the given target *)
  method virtual draw : OcsfmlGraphics.render_target -> TextureLibrary.t ->
    unit

  (** Set the relative position of this widget *)
  method set_position : int * int -> unit

  (** Set the size of this widget *)
  method set_size : int * int -> unit

  (** Get the size of this widget *)
  method get_size : int * int

  (** Set the parent of this widget, should only be used internally *)
  method set_parent : widget option -> unit

end
