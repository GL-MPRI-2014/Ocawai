(* Unit interface (draft) *)

type t

type movement = Walk | Swim | Fly | Amphibious

(* Returns the texture associated to the given unit *)
val get_name : t -> string

(* Returns the type of movement of the given unit *)
val movement_type : t -> movement

val vision_range : t -> int

val attack_range : t -> int

val move_range : t -> int

(* We can also store position in a Unit if necessary
 * (maybe redundant with Map) *)
(* val position : t -> int * int *)

(** Create a unit from the XML file  *)
val create_from_file : string -> string -> t
