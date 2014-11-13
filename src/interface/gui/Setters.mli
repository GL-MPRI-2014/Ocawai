(** Menu items for settings *)

open Settings
open Home

(** Basic setter *)
class virtual setter : (float * float) -> string -> object

  inherit actionnable

  method position : float * float

  method draw : OcsfmlGraphics.render_window -> unit

end

(** A slider
  * ['a] represents the type returned by the slider
  * It takes a function that should associate every integer between [0]
  * and [100] to an ['a] value (basically what is set) *)
class ['a] slider : (float * float) -> (int -> 'a) -> string -> object

  inherit setter
  inherit modal

  method value : 'a

  method draw : OcsfmlGraphics.render_window -> unit

  method action : unit

  method handle_key : OcsfmlWindow.KeyCode.t -> unit

end
