(** Unit interface *)

(**Represent the different type of movement allowed for a unit*)
type movement = Walk | Roll | Tread | Swim | Fly | Amphibious_Walk
  | Amphibious_Roll | Amphibious_Tread | All

(**Represent the different type of armor that a unit can take*)
type armor = Light | Normal | Heavy | Flying | Boat
												 
type unit_id = int


(** Type for units without a position nor id, player_id, life...
    Intended to represent a unit before its instanciation via bind *)
type unbound_t = <
  name : string;
  movement_type : movement;
  vision_range : int;
  min_attack_range : int;
  attack_range : int;
  move_range : int;
  spawn_number : int;
  attack_base : int;
  armor : armor;
  price : int;
  percentage_light : int;
  percentage_normal : int;
  percentage_heavy : int;
  percentage_flying : int;
  percentage_boat : int;
  life_max : int
>
			
(** Unit type *)
type t = <
  name : string;
  position : Position.t;
  get_id : unit_id;
  player_id : int;
  move : Position.t -> unit;
  movement_type : movement;
  vision_range : int;
  min_attack_range : int;
  attack_range : int;
  move_range : int;
  spawn_number : int;
  attack_base : int;
  armor : armor;
  price : int;
  percentage_light : int;
  percentage_normal : int;
  percentage_heavy : int;
  percentage_flying : int;
  percentage_boat : int;
  life_max : int;
  hp : int;
  attack : armor -> int -> int;
  attack_interval : armor -> int -> int * int;
  take_damage : int -> unit;
  has_played : bool;
  set_played : bool -> unit
>

  
(** Create a unit from a unbound unit, a position and the controlling player id *)
val bind : unbound_t -> Position.t -> int -> t

(** Create a unit from a unbound unit, a position, the controlling player id, its hp, an unit id, and has_played, used for Config.config#unit_of_string *)
val bind_extended : unbound_t -> Position.t -> int -> int -> int -> bool -> t

(** Create an unbound unit from a parsed record *)
val create_unbound_from_parsed_unit : Unit_t.t -> unbound_t

(** Create a parsed record from an unbound unit.
  @see 'Config.mli' useful for serialization *)
val create_parsed_unit_from_unbound : unbound_t -> Unit_t.t

