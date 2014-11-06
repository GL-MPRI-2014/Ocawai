(** Tile interface *)

type t

type structure = [ `Block | `Road | `Border of string ]

(** [get_name tile] returns the name of the tile *)
val get_name : t -> string

(** [get_density tile] return the generation factor used by FieldGenerator *)
val get_density : t -> int

(** [get_density tile] return the structure type *)
val get_structure : t -> structure

(** Check if a tile is traversable by a given type of movement *)
val traversable_m : t -> Unit.movement -> bool

(** Check if a tile is traversable by a given unit *)
val traversable : t -> Unit.t -> bool

(** Takes a movement type and return a tile cost. *)
val movement_cost : t -> Unit.movement -> int

(** Takes a unit and return a tile cost. *)
val tile_cost : t -> Unit.t -> int

(** Create a tile based on a json file containing a tile list*)
val create_from_file : string -> string -> t

(** Create a tile based on the tiles config file *)
val create_from_config : string -> t

(** Return the list of tiles *)
val create_list_from_file : string -> t list

(** Return the list of tiles based on the tiles config file *)
val create_list_from_config : unit -> t list

