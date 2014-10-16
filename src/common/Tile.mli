(* Tile interface (draft) *)

type t

(** texture_name tile returns the name of the texture associated to tile *)
val get_name : t -> string

(** Those three functions check if a tile is traversable by a given type of
  * unit. *)
val walkable : t -> bool

val navigable : t -> bool

val flyable : t -> bool

(** Create a tile from the XML file  *)
val create_from_file : string -> string -> t
