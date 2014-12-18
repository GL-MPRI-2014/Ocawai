(** A class representing a tileset (encapsulates a texture and a configuration) *)

type floatrect = {xmin : float; ymin : float; xmax : float; ymax : float}

(** Creates a tileset from a texture and a path to a configuration file *)
class tileset : OcsfmlGraphics.texture -> string -> object

  (** Returns the size of a tileset's tile *)
  method tile_size : int

  (** Returns the tileset's texture *)
  method texture : OcsfmlGraphics.texture

  (** Returns the coordinates associated to the corresponding texture name *)
  method texture_coords : string -> (float * float)

  (** Returns the square containing the corresponding texture name *)
  method texture_rect : string -> floatrect

  (** Returns the int square containing the corresponding texture name *)
  method int_rect : string -> int OcsfmlGraphics.rect

  (** Returns the vao associated to the tileset *)
  method vao : OcsfmlGraphics.vertex_array

end
