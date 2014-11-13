open List

class  player (a : Unit.t list) (b : Building.t list) =
  object (self)
    val mutable army = (a : Unit.t list)
    val mutable buildings = (b : Building.t list)
    val id = Oo.id
    method get_army = army
    method set_army a = army <- a
    method add_unit u = army <- u::army
    method get_buildings = buildings
    method add_building b = buildings <- b::buildings
    method get_next_action = ([]:Action.movement),Action.Wait
  end
  
(*
class virtual player (army_ : Unit.t list) (buildings_ : Building.t list) = 
object (self)
  val mutable army = (army_ : Unit.t list)
  val mutable buildings = (buildings_ : Building.t list)
                          
  method get_army = army
  method add_unit u = army <- u::army
  method set_army army_ = army <- army_
    
  method get_buildings = buildings
  method add_building b = buildings <- b::buildings
  method set_buildings buildings_ = buildings <- buildings_
    
  method virtual get_next_action :  Action.t
end
*)


class dummy_player army_ buildings_ (a: Action.t list) =
  object
    inherit player army_ buildings_
    val mutable actions = (a: Action.t list)
    method get_next_action  =
      if length a == 0 then
        ([Position.create (0,0)], Wait)
      else
        let action= hd(actions) in
        actions<-tl(actions);
        action
  end

type t = player

let create_player () = new dummy_player [] []  []
