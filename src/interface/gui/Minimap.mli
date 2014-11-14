class minimap : int -> int -> int -> object

  method compute : Battlefield.t -> Player.t list -> unit

  method draw : #OcsfmlGraphics.render_target -> Cursor.cursor -> unit

end
