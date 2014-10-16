(* Tile interface (draft) *)

type t
type public =
  | Grass
  | Desert
  | Water
  | Bridge

(** texture_name tile returns the name of the texture associated to tile *)
(* val texture_name : t -> string *)

(** Return the public type *)
val tile_type : t -> public

(** Those three functions check if a tile is traversable by a given type of
  * unit. *)
val walkable : t -> bool

val navigable : t -> bool

val flyable : t -> bool
