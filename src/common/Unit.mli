(* Unit interface (draft) *)

type movement = Walking | Swimming | Flying | Amphibious

type t = <
  name : string;
  position : Position.t;
  movement_type : movement;
  vision_range : int;
  attack_range : int;
  move_range : int
>

val create_from_file : string -> string -> t
