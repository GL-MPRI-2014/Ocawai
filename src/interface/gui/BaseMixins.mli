open OcsfmlWindow

(** Base mixin for all widgets *)
class virtual widget : object

  (** Parent of this widget *)
  val virtual mutable parent : widget option

  (** Position of this widget, relative to its parent *)
  val virtual mutable position : int * int

  (** Size of this widget *)
  val virtual mutable size : int * int


  (** Returns the absolute position of this widget *)
  method position : int * int

  (** Adds an event catcher to this widget *)
  method add_event : (Event.t -> unit) -> unit

  (** Catches the given event *)
  method on_event : Event.t -> unit

  (** Draw the widget to the given target *)
  method virtual draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> 
    unit

end


(** A mixin to handle transformations *)
class virtual transformable : object

  val virtual mutable position : int * int

  val virtual mutable size : int * int

  method set_size : int * int -> unit

  method set_position : int * int -> unit

end


(** A class representing an item (in a menu) *)
class virtual item : string -> string -> (unit -> unit) -> object

  inherit widget

  inherit transformable

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

  method action : unit

end
