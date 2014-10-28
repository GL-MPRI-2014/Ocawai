(** Rendering module *)

(** Draw the Head-User-Display *)
val draw_hud : #OcsfmlGraphics.render_target -> unit

(** Draw the GUI *)
val draw_gui : #OcsfmlGraphics.render_target -> UIManager.ui_manager -> unit

(** Draw the whole game on the screen *)
val render_game : #OcsfmlGraphics.render_target ->
  ClientData.client_data -> unit

(** Load the various ressources stored in ressources/ *)
val load_ressources : unit -> unit
