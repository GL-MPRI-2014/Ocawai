open List

class logicPlayer (a : Unit.t list) (b : Building.t list) =
  object (self)
    val mutable army = (a : Unit.t list)
    val mutable buildings = (b : Building.t list)
                            
    (*Quite dirty mutable id. Can't we do without it ?*)
    val mutable id = 0
    method get_army = army
    method get_id = id
    method set_army a = army <- a
    method add_unit u = army <- u::army
    method set_buildings b = buildings <- b
    method get_buildings = buildings
    method add_building b = buildings <- b::buildings


    (* TODO : implement these methods *)
    method delete_unit (u : Unit.t) =
      let rec delete unit_list =
        match unit_list with
        | [] -> [] (*TO DO: Add an exception*)
        | h::d when h#id == u#id -> d
        | _ -> delete (tl unit_list)
      in
      army <- delete army
        
    method move_unit (u : Unit.t) (p : Action.movement) = ()
    method delete_building (b : Building.t) = ()

    initializer id <- Oo.id self
  end


class virtual player (a : Unit.t list) (b : Building.t list) = 
  object (self) 
  inherit logicPlayer a b
  method virtual get_next_action :  Action.t

end

type t = player
  
class dummy_player army_ buildings_ (a: Action.t list) =
  object
    inherit player army_ buildings_
    val mutable actions = (a: Action.t list)
    method get_next_action  =
      if length a == 0 then
        ([Position.create (0,0)], Action.Wait)
      else
        let action= hd(actions) in
        actions<-tl(actions);
        action
  end

let create_player () = new dummy_player [] []  []
