(** Menu items for settings *)

open Settings
open Home

(** Height of a setter *)
val setter_height : float

(** Basic setter *)
class virtual setter : (float * float) -> string -> object

  inherit modal

  method position : float * float

  method draw : OcsfmlGraphics.render_window -> unit

end

(** A slider
  * It takes a function that should associate every integer between [0]
  * and [100] to a unit value (typically a setter) *)
class slider : (float * float) -> (int -> unit) -> string -> object

  inherit setter

  method draw : OcsfmlGraphics.render_window -> unit

  method action : unit

  method handle_key : OcsfmlWindow.KeyCode.t -> unit

end
