(** About the minimap *)

(** Class representing a minimap *)
class minimap : int -> int -> int -> object

  (** Updates informations on the minimap *)
  method compute : Battlefield.t -> Player.logicPlayer list -> unit

  (** Draws the minimap *)
  method draw : #OcsfmlGraphics.render_target -> Cursor.cursor -> unit

end
