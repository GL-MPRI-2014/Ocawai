(** BattleField Generator and armies spawner *)

exception GeneratorFailure

(** [new t width height nbplayers gen_attempts units_attempts] generate an object
  containing a Battlefield with size [width * height],
  an armies list containing [nbplayers] Unit lists, 
  and a armies spawns list containing [nbplayers] positions.
  The generation tries gen_attempts times, and each time tries
  units_attempts times to spawn nbplayer armies.
  raise GeneratorFailure if all attempts failed *)
class t : int -> int -> int -> int -> int -> object
  method field : Battlefield.t
  method armies : Unit.t list list
  method spawns : Position.t list
end

