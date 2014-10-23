(** Map interface (draft) *)

type t

(** Creates a new field *)
val create : int -> int -> Tile.t -> t

(** [get_tile map x y] returns the tile at position [(x,y)] on [map] *)
val get_tile : t -> Position.t -> Tile.t

val set_tile : t -> Position.t -> Tile.t -> unit

val tile_iter : (Tile.t -> unit) -> t -> unit

val tile_iteri : (Position.t -> Tile.t -> unit) -> t -> unit

(** In order to have a simple map for testing *)
val dummy_map : unit -> t
