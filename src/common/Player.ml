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

    (*Quite dirty mutable id. Can't we do without it ?*)
    val mutable id_ =
      match id with
      | None -> 0
      | Some(id__) -> id__

    method get_army =
      Hashtbl.fold (fun id u l -> u::l) army []

    method get_id = id_

    method set_army a =
      List.iter (fun unit -> self#add_unit unit) a

    method add_unit u = Hashtbl.add army u#get_id u
        (*TODO*)
    method set_buildings b =
      List.iter (fun building -> self#add_building building) b
    method get_buildings =
      Hashtbl.fold (fun id b l -> b::l) buildings []
    method get_base = base
    method set_base b = base <- Some b

    method add_building b = Hashtbl.add buildings b#get_id b

    (* TODO *)
    method set_unit_hp (u : Unit.id) (h : int) = ()

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
      self#log (Moved (u, p));
      u#move (final_position (get_path p))

    method delete_building (id_building : Building.id) =
      try
        ignore(Hashtbl.find buildings id_building);
          Hashtbl.remove buildings id_building
      with Not_found -> raise Not_found

    method get_value_resource = resource

    method use_resource amount = if resource < amount then false else ( resource <- resource - amount;true)

    method harvest_buildings_income = List.iter (fun b -> resource <- max 0 (resource + b#income)) self#get_buildings

    initializer
      match id with
      | None -> id_ <- Oo.id self;
      | _ -> ();
      List.iter (fun unit -> self#add_unit unit) a;
      List.iter (fun building -> self#add_building building) b;
  end


class virtual player  ?(id) (a : Unit.t list) (b : Building.t list) =
  object (self)
  inherit logicPlayer ?id:id a b
  val mutable logic_player_list:logicPlayer list = []
  method virtual get_next_action :  Action.t
  method set_logic_player_list playerList = logic_player_list <- playerList
  method get_logic_player_list = logic_player_list
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

  end

let create_player () = new dummy_player [] []  []
let create_dummy_player actions = new dummy_player [] [] actions
