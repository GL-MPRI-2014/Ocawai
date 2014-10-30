(** Module for menus *)

(** A class representing an item (in a menu) *)
class item : string -> string -> (unit -> unit) -> object

  inherit Widget.widget

  val mutable position : int * int

  val mutable size : int * int

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

  method action : unit

end

(** A class representing key-controlled buttons *)
class key_button : icon:string -> text:string -> m_position:(int*int) ->
  m_size:(int*int) -> keycode:OcsfmlWindow.KeyCode.t ->
  callback:(unit -> unit) -> m_theme:Theme.t -> object

  inherit Widget.widget

  val mutable position : int * int

  val mutable size : int * int

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

end


(** Same as key_button, but togges itself after usage *)
class key_button_oneuse : icon:string -> text:string -> m_position:(int*int) 
  -> m_size:(int*int) -> keycode:OcsfmlWindow.KeyCode.t ->
  callback:(unit -> unit) -> m_theme:Theme.t -> object

  inherit key_button

end


(** Usage : menu position width item_height theme *)
class menu : (int * int) -> int -> int -> OcsfmlWindow.KeyCode.t -> 
  Theme.t -> object

  inherit [item] BaseMixins.evq_container 

  inherit BaseMixins.key_ctrl_list

  val mutable position : int * int

  val mutable size : int * int

  val mutable nb_items : int

  val mutable item_height : int

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

end
