open OcsfmlWindow
open OcsfmlGraphics
open Utils

class virtual widget = object(self)

  val mutable parent : widget option = None

  val virtual mutable position : (int * int)

  val virtual mutable size : (int * int)

  val mutable active = false

  val mutable event_funs : (Event.t -> unit) list = []

  method position =
    match parent with
    | None -> position
    | Some(s) -> add2D s#position position

  method add_event f = event_funs <- f :: event_funs

  method on_event e = if active then List.iter (fun f -> f e) event_funs

  method set_position p = position <- p

  method set_size s = size <- s

  method get_size = size

  method set_parent p = parent <- p

  method virtual draw : render_target -> TextureLibrary.t -> unit

  method toggle = active <- not active

  method active = active

end
