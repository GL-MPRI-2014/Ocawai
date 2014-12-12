(** Battlefield operations *)

(** battlefield type*)
type t

(** Return the dimensions of a given battlefield*)
val size : t -> int * int

(** [create w h t] creates a new battlefield of size [w]*[h] filled with tiles [t] *)
val create : int -> int -> Tile.t -> t

(** [get_tile map p] returns the tile at position [p] in [map] *)
val get_tile : t -> Position.t -> Tile.t

(** [set_tile map p t] places the tile [t] at position [p] in [map] *)
val set_tile : t -> Position.t -> Tile.t -> unit

(** [tile_iter f map] applies [f] to every tile of [map] *)
val tile_iter : (Tile.t -> unit) -> t -> unit

(** Same as {!tile_iter}, but the function is applied to the position of
the element as first argument (counting from 0), and the element
itself as second argument*)
val tile_iteri : (Position.t -> Tile.t -> unit) -> t -> unit

(** [tile_filter f map] returns all the elements of the battlefield [map]
that satisfy the predicate [f]*)
val tile_filter : (Tile.t -> bool) -> t -> Position.t list

(** Same as {!tile_filter}, but the function is applied to the position of
the element as first argument (counting from 0), and the element
itself as second argument*)
val tile_filteri : (Position.t -> Tile.t -> bool) -> t -> Position.t list

(** Returns true iff the given position is within the battlefield *)
val in_range : t -> Position.t -> bool

