(** A class representing a tileset (encapsulates a texture and a configuration) *)

(** Creates a tileset from a texture and a path to a configuration file *)
class tileset : OcsfmlGraphics.texture -> string -> object

  (** Returns the size of a tileset's tile *)
  method tile_size : int

  (** Returns the tileset's texture *)
  method texture : OcsfmlGraphics.texture

  (** Returns the coordinates associated to the corresponding texture name *)
  method texture_coords : string -> (int * int)

  (** Returns the square containing the corresponding texture name *)
  method texture_rect : string -> int OcsfmlGraphics.rect

end
