class minimap : int -> int -> int -> object

  method compute : Battlefield.t -> Player.logicPlayer list -> unit

  method draw : #OcsfmlGraphics.render_target -> Cursor.cursor -> unit

end
