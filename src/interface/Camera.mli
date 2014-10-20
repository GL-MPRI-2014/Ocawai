class camera : tile_size:int -> w:int -> h:int -> maxpos:Position.t -> object

  method cursor : Cursor.cursor

  method project : Position.t -> (int * int)

  method top_left : Position.t

  method bottom_right : Position.t

  method tile_size : int

  method move : float * float -> unit

end
