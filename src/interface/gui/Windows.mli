(** Defining dialog windows *)

(** Creates a simple item without icon and a colored frame *)
class text_framed_item : (int * int) -> (int * int) -> string ->
  (unit -> unit) -> Theme.t -> object

    inherit Widget.widget

    val mutable position : int * int

    val mutable size : int * int

    method action : unit

    method draw : OcsfmlGraphics.render_window -> TextureLibrary.t -> unit

end


(** Creates an ingame popup window *)
class ingame_popup : m_position:(int * int) -> m_size:(int * int) ->
  m_theme:Theme.t -> m_text:string -> m_bar_height:int ->
  m_bar_icon:string -> m_bar_text:string ->
    object

  inherit [text_framed_item] BaseMixins.widget_container

  inherit BaseMixins.key_ctrl_list

  val mutable position : int * int

  val mutable size : int * int

  val mutable nb_items : int

  method draw : OcsfmlGraphics.render_window -> TextureLibrary.t -> unit

end
