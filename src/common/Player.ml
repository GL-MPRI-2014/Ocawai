open List
open Path

class logicPlayer ?(id_) (a : Unit.t list) (b : Building.t list) =
  object (self)
    val mutable army = Hashtbl.create 97
    val mutable buildings = Hashtbl.create 23
                            
    (*Quite dirty mutable id. Can't we do without it ?*)
    val mutable id =
      match id_ with
      | None -> 0
      | Some(id) -> id 

    method get_army =
      Hashtbl.fold (fun id u l -> u::l) army []
        
    method get_id = id

    method set_army a =
      List.iter (fun unit -> self#add_unit unit) a
        
    method add_unit u = Hashtbl.add army u#get_id u
        (*TODO*)
    method set_buildings b = 
      List.iter (fun building -> self#add_building building) b
    method get_buildings =
      Hashtbl.fold (fun id b l -> b::l) buildings []

    method add_building b = Hashtbl.add buildings b#get_id b


    method delete_unit (id_unit : Unit.id) =
      try
        ignore(Hashtbl.find army id_unit);
          Hashtbl.remove army id_unit
      with Not_found -> raise Not_found
      
    method get_unit_by_id (id_unit : Unit.id) = Hashtbl.find army id_unit
    method get_building_by_id (id_building : Building.id) = Hashtbl.find buildings id_building
        
    (*it is quite dirty*)
    method move_unit (id_unit : Unit.id) (p : Action.movement) =
      let u = self#get_unit_by_id id_unit in
      u#move (final_position (get_path p))


    method delete_building (id_building : Building.id) =
      try
        ignore(Hashtbl.find buildings id_building);
          Hashtbl.remove buildings id_building
      with Not_found -> raise Not_found
      
    initializer
      match id_ with 
      | None -> id <- Oo.id self;
      | _ -> ();
      List.iter (fun unit -> self#add_unit unit) a;
      List.iter (fun building -> self#add_building building) b;
  end


class virtual player (a : Unit.t list) (b : Building.t list) = 
  object (self) 
  inherit logicPlayer a b
  val mutable logicPlayerList = [] 
  method virtual get_next_action :  Action.t
  method virtual set_logicPlayerList : (logicPlayer list) -> unit
  method virtual get_logicPlayerList : logicPlayer list
  method  update (u:Types.update) =
    ()
end

type t = player


class dummy_player army_ buildings_ (a: Action.t list) =
  object
    inherit player army_ buildings_
    val mutable actions = (a: Action.t list)
    method get_next_action  =
      if length a == 0 then
        ([], Action.End_turn)
      else
        let action= hd(actions) in
        actions<-tl(actions);
        action

    method set_logicPlayerList playersList =
	()

    method get_logicPlayerList =
	logicPlayerList
  end

let create_player () = new dummy_player [] []  []
let create_dummy_player actions = new dummy_player [] [] actions
