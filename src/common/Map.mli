(* Map interface (draft) *)

type t

(** get_tile map x y returns the tile at position (x,y) on map *)
val get_tile : t -> int -> int -> Tile.t

(** get_unit map x y returns the unit at position (x,y) on map, if any *)
val get_unit : t -> int -> int -> Unit.t option 

(** Same, but if we allow multiple units to share the same tile *)
(* get_unit : t -> int -> int -> Unit.t list *)

val get_building : t -> int -> int -> Building.t option
