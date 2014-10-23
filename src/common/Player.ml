class player = 
object (self)
  val mutable army = ([] : Unit.t list)
  val mutable buildings = ([] : Building.t list)
  method get_army = army
  method add_unit u = army <- u::army
  method get_buildings = buildings
  method add_building b = buildings <- b::buildings
  method get_next_action = ((0,0),Wait)
end

type t = player
