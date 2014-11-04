(** Home screen and menus (before game) *)

(* TODO inherit from BaseMixins *)

(** A screen item *)
class item : string -> (float * float) -> object

  method draw : OcsfmlGraphics.render_window -> unit

end

(** An actionnable item *)
class actionnable : string -> string -> (float*float) -> (unit -> unit) ->
object

  inherit item

  method action : unit

  method set_selected : bool -> unit

  method draw : OcsfmlGraphics.render_window -> unit

end

(** A screen (simply a menu)
  * It has the particularity it handles selection with position *)
class screen : item list -> actionnable list -> object

  method draw : OcsfmlGraphics.render_window -> unit

end
