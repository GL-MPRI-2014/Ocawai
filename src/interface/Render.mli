
val render_map : #OcsfmlGraphics.render_target -> Camera.camera ->
  Battlefield.t -> unit

val draw_path : #OcsfmlGraphics.render_target -> Camera.camera ->
  Action.movement -> unit

(* Not a good type, just for testing *)
val draw_units : #OcsfmlGraphics.render_target -> Camera.camera -> unit

val draw_hud : #OcsfmlGraphics.render_target -> unit
