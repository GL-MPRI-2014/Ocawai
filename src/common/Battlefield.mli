(** Battlefield interface *)

type t

(** Creates a new field *)
val create : int -> int -> Tile.t -> t

(** [get_tile map p] returns the tile at position [p] on [map] *)

val get_tile : t -> Position.t -> Tile.t

val set_tile : t -> Position.t -> Tile.t -> unit

val tile_iter : (Tile.t -> unit) -> t -> unit

val tile_iteri : (Position.t -> Tile.t -> unit) -> t -> unit

val size : t -> int * int

(** Returns true iff the given position is within the battlefield *)
val in_range : t -> Position.t -> bool
