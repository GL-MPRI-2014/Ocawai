class camera : tile_size:int -> w:int -> h:int -> object

  method set_cursor : Position.t -> unit

  method cursor : Position.t

  method project : Position.t -> (int * int)

  method top_left : Position.t

  method bottom_right : Position.t

  method tile_size : int

end
