(** Rendering module *)

(** The renderer in itself *)
val renderer : <

  (** Draw a tile from a tileset *)
  draw_direct_tile : OcsfmlGraphics.render_window -> Tileset.tileset ->
    string ->
    ?position:(float * float) ->
    ?rotation:float ->
    ?scale:(float*float) ->
    ?color:OcsfmlGraphics.Color.t ->
    ?origin:(float*float) -> unit -> unit;

  (** Draw a [texture] in screen coordinates (from the local library).
    * Usage: [draw_txr target name ... ]*)
  draw_txr : OcsfmlGraphics.render_window -> string ->
    ?position:(float * float) ->
    ?rotation:float ->
    ?scale:(float*float) ->
    ?size:(float*float) ->
    ?color:OcsfmlGraphics.Color.t ->
    ?centered:bool ->
    ?blend_mode:OcsfmlGraphics.blend_mode -> unit -> unit;

  (** Draw the GUI *)
  draw_gui : OcsfmlGraphics.render_window ->
    UIManager.ui_manager -> unit;

  (** Draw the whole game on the screen *)
  render_game : OcsfmlGraphics.render_window ->
    ClientData.client_data -> Updates.handler -> unit;

  (** Load the various ressources stored in ressources/ *)
  init : unit

>
