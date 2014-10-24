(** Module for menus *)

(** A class representing an item (in a menu) *)
class item : string -> string -> (unit -> unit) -> object

  inherit Widget.widget

  val mutable position : int * int

  val mutable size : int * int

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

  method action : unit

end

(** Usage : menu position width item_height *)
class menu : (int * int) -> int -> int -> object

  inherit [item] BaseMixins.evq_container 

  inherit BaseMixins.key_ctrl_list

  val mutable position : int * int

  val mutable size : int * int

  val mutable nb_items : int

  val mutable item_height : int

  method draw : OcsfmlGraphics.render_target -> TextureLibrary.t -> unit

end
