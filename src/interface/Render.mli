(** Rendering module *)

(** Draw the Head-User-Display *)
val draw_hud : #OcsfmlGraphics.render_target -> unit

(** Draw the whole game on the screen *)
val render_game : #OcsfmlGraphics.render_target ->
  ClientData.client_data -> unit

val render_widget : #OcsfmlGraphics.render_target ->
  #Widget.widget -> unit

(** Load the various ressources stored in ressources/ *)
val load_ressources : unit -> unit
