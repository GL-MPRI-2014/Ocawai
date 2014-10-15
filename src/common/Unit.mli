(* Unit interface (draft) *)

type t

(* Public type for units as an algebraic type -- draft *)
type public =
  | Infantry
  | Mech
  | Constructor
  | Recon
  | Tank

type movement = Walk | Swim | Fly | Amphibie

(* Returns the texture associated to the given unit *)
(* val texture_name : t -> string *)

(* Returns the public type associated *)
val unit_type : t -> public

(* Returns the type of movement of the given unit *)
val movement_type : t -> movement

val vision_range : t -> int

val attack_range : t -> int

val move_range : t -> int

(* We can also store position in a Unit if necessary
 * (maybe redundant with Map) *)
(* val position : t -> int * int *)
