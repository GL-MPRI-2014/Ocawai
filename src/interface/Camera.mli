(** Camera for displaying *)
class camera : tile_size:int -> w:int -> h:int -> maxpos:Position.t -> object

  method cursor : Cursor.cursor

  method project : Position.t -> (int * int)

  method top_left : Position.t

  method bottom_right : Position.t

  method tile_size : int

  (** [move v moves] the camera by adding [v] to its current position *)
  method move : (int*int) -> unit

  (** [set_position v] moves the camera by setting its current position to
   *  [v] *)
  method set_position : Position.t -> unit

end
