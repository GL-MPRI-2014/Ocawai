(** Unit interface *)

type movement = Walk | Roll | Tracks | Swim | Fly | Amphibious_Walk | Amphibious_Roll | Amphibious_Tracks

type t = <
  name : string;
  position : Position.t;
  move : Position.t -> unit;
  movement_type : movement;
  vision_range : int;
  attack_range : int;
  move_range : int;
  spawn_number : int
>

val create_from_unit_t : Unit_t.t -> Position.t ->t

(** Create a unit from the XML file  *)
val create_from_file : string -> string -> Position.t -> t

val create_from_config : string -> Position.t -> t
