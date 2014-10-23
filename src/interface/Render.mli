(*À changer par l'équipe interface pour respecter le paradigme objet de Unit*)

val draw_hud : #OcsfmlGraphics.render_target -> unit

val render_game : #OcsfmlGraphics.render_target -> 
  ClientData.client_data -> unit
