(* Map interface (draft) *)

type t

(** get_tile map x y returns the tile at position (x,y) on map *)
val get_tile : t -> Position.t -> Tile.t

val set_tile : t -> Position.t -> Tile.t -> unit

