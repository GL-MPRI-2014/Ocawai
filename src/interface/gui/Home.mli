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
