class player = 
object (self)
  method get_army = Unit.t list
  method add_unit u = unit
  method get_buildings = Building.t
  method add_building b = unit
  method get_next_action = (Action.movement * Action.action)
end

type t = player
