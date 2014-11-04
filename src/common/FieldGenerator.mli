(** BattleField Generator and armies spawner *)

(** [new t width height nbplayers] generate an object containing a Battlefield with size [width * height], an armies list containing [nbplayers] Unit lists, and a armies spawns list containing [nbplayers] positions *)

class t : int -> int -> int -> int -> int -> object
  method field : Battlefield.t
  method armies : Unit.t list list
  method spawns : Position.t list
end

