(* Tile interface *)

type t

(** [get_name] tile returns the name of the tile *)

val get_name : t -> string

val get_densite : t -> int

(** Check if a tile is traversable by a given type of movement/unit *)

val traversable_m : t -> Unit.movement -> bool

val traversable : t -> Unit.t -> bool

(** Takes a movement type/unit and return a tile cost. *)

<<<<<<< HEAD
val movement_cost : t -> Unit.movement -> int

val tile_cost : t -> Unit.t -> int

(** Create a tile from the json file  *)

val tile_t_to_t : Tile_t.t -> t

=======
(** Takes a movement type and return a tile cost. [Min(Walk,Swim)]
  * if [Amphibious] *)
val movement_cost : t -> Unit.movement -> int

(** Create a tile from the XML file *)
>>>>>>> master
val create_from_file : string -> string -> t

val create_from_config : string -> t
