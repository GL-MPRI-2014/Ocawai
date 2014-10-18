
val render_map : #OcsfmlGraphics.render_target -> Camera.camera ->
  Battlefield.t -> unit

val draw_path : #OcsfmlGraphics.render_target -> Camera.camera ->
  Action.movement -> unit

val draw_hud : #OcsfmlGraphics.render_target -> unit
