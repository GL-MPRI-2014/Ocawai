class player (a : Unit.t list) (b : Building.t list) = 
object (self)
  val mutable army = (a : Unit.t list)
  val mutable buildings = (b : Building.t list)
  method get_army = army
  method set_army a = army <- a

  method add_unit u = army <- u::army
                              
  
  method get_buildings = buildings

  method add_building b = buildings <- b::buildings
  method set_buildings (buildings: Building.t list) =();

  method get_next_action = ([]:Action.movement),Action.Wait
  method push_next_action (action:Action.t) = ()

  method last_game_update = ([]:Action.movement),Action.Wait
  method push_last_game_update (action:Action.t) = ()
end

type t = player

let create_player () = new player [] [] 
