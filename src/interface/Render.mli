(** Rendering module *)

(** Draw a [texture] in screen coordinates (from the local library).
  * Usage: [draw_txr target name position rotation]*)
val draw_txr : #OcsfmlGraphics.render_target -> string ->
  (float * float) -> ?rotation:float-> ?scale:(float*float) -> 
  ?blend_mode:OcsfmlGraphics.blend_mode -> ?color:OcsfmlGraphics.Color.t -> 
    unit -> unit

(** Draw the Head-User-Display *)
val draw_hud : #OcsfmlGraphics.render_target -> unit

(** Draw the GUI *)
val draw_gui : #OcsfmlGraphics.render_target -> UIManager.ui_manager -> unit

(** Draw the whole game on the screen *)
val render_game : #OcsfmlGraphics.render_target ->
  ClientData.client_data -> unit

(** Load the various ressources stored in ressources/ *)
val load_ressources : unit -> unit
