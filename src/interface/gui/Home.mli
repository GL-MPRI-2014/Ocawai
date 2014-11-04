(** Home screen and menus (before game) *)

(** A screen item *)
class item : object

  method draw : OcsfmlGraphics.render_target -> unit

end

(** An actionnable item *)
class actionnable : (unit -> unit) -> object

  inherit item

  method action : unit

end

(** A screen (simply a menu)
  * It has the particularity it handles selection with position *)
class screen : item list -> actionnable list -> object

  method draw : OcsfmlGraphics.render_target -> unit

end
