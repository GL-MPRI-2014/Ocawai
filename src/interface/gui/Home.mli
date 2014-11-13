(** Home screen and menus (before game) *)

(* TODO inherit from BaseMixins *)

(** An item for the screen
  * It has a [position] that represents its center *)
class virtual item : object

  method virtual draw : OcsfmlGraphics.render_window -> unit

  method virtual position : float * float
  method x : float
  method y : float

end

(** An actionnable item *)
class virtual actionnable : object

  inherit item

  method set_focus : bool -> unit

  method virtual action : unit

end

(** A modal item
  * It retains the focus when activated *)
class virtual modal : object

  inherit actionnable

  method holds_focus : bool
  method virtual handle_key : OcsfmlWindow.KeyCode.t -> unit

end

(** A screen item
  * [position] should be given wrt to the center of the texture *)
class textured_item : string -> (float * float) -> object

  inherit item

  method draw : OcsfmlGraphics.render_window -> unit

  method position : float * float

end

(** An actionnable item *)
class textured_actionnable : string -> string -> (float*float) -> (unit -> unit) ->
object

  inherit textured_item
  inherit actionnable

  method action : unit

  method draw : OcsfmlGraphics.render_window -> unit

end

(** A screen (simply a menu)
  * It has the particularity it handles selection with position.
  * TODO: Handle intro; outro; multiple textures and interpolators *)
class screen : item list -> actionnable list -> object

  method draw : OcsfmlGraphics.render_window -> unit

  method handle_key : OcsfmlWindow.KeyCode.t -> unit

end
