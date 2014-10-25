(** Unit interface *)

type movement = Walk | Roll | Tracks | Swim | Fly | Amphibious_Walk | Amphibious_Roll | Amphibious_Tracks

type t = <
  name : string;
  position : Position.t;
  movement_type : movement;
  vision_range : int;
  attack_range : int;
  move_range : int
>

(** Create a unit from the XML file  *)

val create_from_file : string -> string -> Position.t -> t
