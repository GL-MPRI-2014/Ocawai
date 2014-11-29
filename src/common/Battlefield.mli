(** Battlefield interface *)

type t

(** Creates a new field *)
val create : int -> int -> Tile.t -> t

(** [get_tile map p] returns the tile at position [p] on [map] *)

val get_tile : t -> Position.t -> Tile.t

val set_tile : t -> Position.t -> Tile.t -> unit

val tile_iter : (Tile.t -> unit) -> t -> unit

val tile_iteri : (Position.t -> Tile.t -> unit) -> t -> unit

val tile_filter : (Tile.t -> bool) -> t -> Position.t list

val tile_filteri : (Position.t -> Tile.t -> bool) -> t -> Position.t list

val size : t -> int * int

(** Returns true iff the given position is within the battlefield *)
val in_range : t -> Position.t -> bool

(** [to_string map] Returns a string s such that , if [size map] = [(width,height)] then [create_from_string width height s] returns [map] *)
val to_string : t -> Config.t -> string

(** [create_from_string width height s] creates a new field based on [s], a [to_string] output *)
val create_from_string : int -> int -> string -> Config.t -> t
