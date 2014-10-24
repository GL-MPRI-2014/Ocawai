(* Unit interface (draft) *)

type movement = Walk | Roll | Tracks | Swim | Fly | Amphibious_Walk | Amphibious_Roll | Amphibious_Tracks

type t = <
  name : string;
  position : Position.t;
  movement_type : movement;
  vision_range : int;
  attack_range : int;
  move_range : int
>

val create_from_file : string -> string -> t
