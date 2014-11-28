(** Menu items for settings *)

open Settings
open Home

(** Height of a setter *)
val setter_height : float

(** Basic setter
  * @param pos the center of the setter
  * @param name name of the settings it represents *)
class virtual setter : (float * float) -> string -> object

  inherit modal

  method position : float * float

  method draw : OcsfmlGraphics.render_window -> unit

end

(** Slider (for 'continuous' parameters)
  * @param default (optionnal) default value of the setter between [0] and [100]
  * @param pos the center position
  * @param update a function that updates the setting called everytime the
  * slider changes, the [int] given as argument is between [0] and [100]
  * @param name the name of the parameter *)
class slider : ?default:int -> (float * float) -> (int -> unit) -> string ->
object

  inherit setter

  method draw : OcsfmlGraphics.render_window -> unit

  method action : unit

  method handle_key : OcsfmlWindow.KeyCode.t -> unit

end

(** Toogle (typically for boolean parameters)
  * @param default the default value (on/off)
  * @param pos the center position of the setter
  * @param name name of the parameter
  * @param update a function taking a [bool] as parameter called at every
  * toogle *)
class toogle : ?default:bool -> (float * float) -> string -> (bool -> unit) ->
object

  inherit setter

  method draw : OcsfmlGraphics.render_window -> unit
  method action : unit

end
