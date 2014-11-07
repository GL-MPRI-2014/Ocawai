(** Unit interface *)

type movement = Walk | Roll | Tread | Swim | Fly | Amphibious_Walk | Amphibious_Roll | Amphibious_Tread

(** Unit type *)
type t = <
  name : string;
  position : Position.t;
  move : Position.t -> unit;
  movement_type : movement;
  vision_range : int;
  min_attack_range : int;
  attack_range : int;
  move_range : int;
  spawn_number : int
>

(** Type for units without a position *)
type unbound_t = <
  name : string;
  movement_type : movement;
  vision_range : int;
  min_attack_range : int;
  attack_range : int;
  move_range : int;
  spawn_number : int
>

(** Create a unit from a unbound unit and a position *)
val bind : unbound_t -> Position.t -> t

(** conversion from parsed format from json config files *)
val create_from_unit_t : Unit_t.t -> Position.t ->t

(** Create a unit based on a json file containing a unit list*)
val create_from_file : string -> string -> Position.t -> t

(** Create a unit based on the the units config file *)
val create_from_config : string -> Position.t -> t
