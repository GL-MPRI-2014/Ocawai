open List
open Path

type log_item =
  | Moved of Unit.t * Action.movement

class logicPlayer ?(id) (a : Unit.t list) (b : Building.t list) =
  object (self)

    val mutable log : (int * log_item) list = []
    val mutable log_c = 0

    method private log action =
      log <- (log_c, action) :: log ;
      log_c <- log_c + 1

    method get_log = log

    val mutable army = Hashtbl.create 97
    val mutable buildings = Hashtbl.create 23
    val mutable resource = 0
    val mutable base : Building.t option = None
    val mutable fog = [||]
    (*Quite dirty mutable id. Can't we do without it ?*)
    val mutable id_ =
      match id with
      | None -> 0
      | Some(id__) -> id__

    method get_army =
      Hashtbl.fold (fun id u l -> u::l) army []

    method get_visible_army_for (p:logicPlayer) =
        if Array.length fog > 0 then 
            Fog.visible_army p#get_fog self#get_army
        else
            self#get_army
    method get_id = id_

    method get_fog = fog

    method set_army a =
      List.iter (fun unit -> self#add_unit unit) a;
      if Array.length fog > 0 then List.iter (fun x -> Fog.add_unit_fog fog x#position x#vision_range) a

    method add_unit u = 
        Hashtbl.add army u#get_id u;
        if Array.length fog > 0 then Fog.add_unit_fog fog u#position u#vision_range
        (*TODO*)

    method set_buildings b =
      List.iter (fun building -> self#add_building building) b
    method get_buildings =
      Hashtbl.fold (fun id b l -> b::l) buildings []
    method get_base = base
    method set_base b = base <- Some b

    method add_building (b:Building.t) = Hashtbl.add buildings b#get_id b

    (* TODO *)
    method set_unit_hp (u : Unit.id) (h : int) = ()

    method delete_unit (id_unit : Unit.id) =
      let u = (Hashtbl.find army id_unit) in
      if Array.length fog > 0 then Fog.delete_unit_fog fog u#position u#vision_range;
      Hashtbl.remove army id_unit


    method get_unit_by_id (id_unit : Unit.id) = Hashtbl.find army id_unit
    method get_building_by_id (id_building : Building.id) = Hashtbl.find buildings id_building

    (*it is quite dirty*)
    method move_unit (id_unit : Unit.id) (p : Action.movement) =
      let u = self#get_unit_by_id id_unit in
      if Array.length fog > 0 then Fog.delete_unit_fog fog u#position u#vision_range;
      self#log (Moved (u, p));
      u#move (final_position (get_path p));
      if Array.length fog > 0 then Fog.add_unit_fog fog u#position u#vision_range

    method delete_building (id_building : Building.id) =
      try
        ignore(Hashtbl.find buildings id_building);
          Hashtbl.remove buildings id_building
      with Not_found -> raise Not_found

    method get_value_resource = resource

    method use_resource amount = if resource < amount then false else ( resource <- resource - amount;true)

    method harvest_buildings_income = List.iter (fun b -> resource <- max 0 (resource + b#income)) self#get_buildings

    method init (field: Battlefield.t) (players:logicPlayer list) =  
        let (size_x,size_y) = Battlefield.size field in
        fog <- Array.make_matrix size_x size_y 0;
        List.iter (fun x -> Fog.add_unit_fog fog x#position x#vision_range) self#get_army;


    initializer
      match id with
      | None -> id_ <- Oo.id self;
      | _ -> ();
      (* Useless initialisation *)
     (* List.iter (fun unit -> self#add_unit unit) a;
      List.iter (fun building -> self#add_building building) b;*)
  end


class virtual player  ?(id) (a : Unit.t list) (b : Building.t list) =
  object (self)
  inherit logicPlayer ?id:id a b
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
