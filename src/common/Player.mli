<<<<<<< HEAD
class player :
object (self)
  method get_army : Unit.t list
  method add_unit : Unit.t -> unit
  method get_buildings : Building.t list
  method add_building : Building.t -> unit
  method get_next_action : Action.t
end

type t = player

