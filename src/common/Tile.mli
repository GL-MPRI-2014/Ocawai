(** Tile interface *)

type t = Tile_t.t

(** {Border : name, rate, expansion} *)
type structure = [ `Block | `Road | `Border of (string * int * int) ]

(** [get_name tile] returns the name of the tile *)
val get_name : t -> string

(** [get_density tile] returns the generation factor used by FieldGenerator *)
val get_density : t -> int

(** [get_frow_speed tile] returns the expantion speed of [tile] used by the generation by seeds *)
val get_grow_speed : t -> int

(** [get_structure tile] returns the structure type *)
val get_structure : t -> structure

(** Check if a tile is traversable by a given type of movement *)
val traversable_m : t -> Unit.movement -> bool

(** Check if a tile is traversable by a given unit *)
val traversable : t -> Unit.t -> bool

(** compare two tiles in term of movements.
  [compare_movements t1 t2] is: 
    Some i <=0 if t1 possible movements are included in t2 possible movements
    Some i >=0 if t2 possible movements are included in t1 possible movements
    None if their possiblities are not comparable *)
val compare_movements : t -> t -> int option

(** compare two tiles in term of Walk, Roll and Tread movements.
  [compare_walkability t1 t2] is: 
    Some i <=0 if t1 ground movements are included in t2 ground movements
    Some i >=0 if t2 ground movements are included in t1 ground movements
    None if their possiblities are not comparable *)
val compare_walkability : t -> t -> int option

(** Takes a movement type and return a tile cost. *)
val movement_cost : t -> Unit.movement -> int

(** Takes a unit and return a tile cost. *)
val tile_cost : t -> Unit.t -> int

