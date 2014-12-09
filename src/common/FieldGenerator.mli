(** BattleField Generator and armies spawner *)

(** raised when the generation fails*)
exception GeneratorFailure

(** [new t players_list] generates an object
    containing a battlefield, an army list and a spawn position list coresponding to the armies.
    
    The battlefield dimensions and the parameters used for the generation are supposed properly set in [Config.config#settings] and [Config.config#settings_engine].
    
    Returned armies are assigned to the players in [players_list] in the construction of [t], they do not need to be added to them*)
class t : Player.logicPlayer list -> object
  method field : Battlefield.t
  method armies : Unit.t list list
  method spawns : Position.t list
end
