open List
open Path


exception Not_enough_resources


class logicPlayer ?id () =
  object (self)

    val army = Hashtbl.create 97
    val buildings = Hashtbl.create 23
    val mutable resource = 0
    val mutable base : Building.t option = None
    val mutable fog = [||]
    (*Quite dirty mutable id. Can't we do without it ?*)
    val mutable id_ =
      match id with
      | None -> 0
      | Some(id__) -> id__

    method has_playable_unit =
      (* List.fold_left (fun b u -> b || (not u#has_played)) false (self#get_army) *)
      List.exists (fun u -> not u#has_played) self#get_army

    method get_army =
      Hashtbl.fold (fun id u l -> u::l) army []

    method get_visible_army_for (p:logicPlayer) =
        if Array.length fog > 0 then
            Fog.visible_army p#get_fog self#get_army
        else
            self#get_army

    method get_visible_buildings_for (p:logicPlayer) =
        if Array.length fog > 0 then
            Fog.visible_buildings p#get_fog self#get_buildings
        else
            self#get_buildings

    method get_id = id_

    method get_fog = fog

    method set_army a =
      List.iter (fun u -> self#add_unit u) a;
      if Array.length fog > 0 then
        List.iter (fun x -> Fog.add_unit_fog fog x#position x#vision_range) a

    method add_unit u =
        Hashtbl.replace army u#get_id u;
        if Array.length fog > 0 then
          Fog.add_unit_fog fog u#position u#vision_range
        (*TODO*)

    method set_buildings b =
      List.iter (fun building -> self#add_building building) b

    method get_buildings =
      Hashtbl.fold (fun id b l -> b::l) buildings []

    method get_base = base

    method set_base b = base <- Some b

    method add_building (b:Building.t) =
        Hashtbl.replace buildings b#get_id b;
        if Array.length fog > 0 then
          Fog.add_unit_fog fog b#position b#vision_range

    (* TODO *)
    method set_unit_hp (u : Unit.unit_id) (h : int) = ()

    method delete_unit (id_unit : Unit.unit_id) =
      let u = (Hashtbl.find army id_unit) in
      if Array.length fog > 0 then
        Fog.delete_unit_fog fog u#position u#vision_range;
      Hashtbl.remove army id_unit

    method get_unit_by_id (id_unit : Unit.unit_id) =
      Hashtbl.find army id_unit

    method get_building_by_id (id_building : Building.building_id) =
      Hashtbl.find buildings id_building


    method move_unit (id_unit : Unit.unit_id) (p : Action.movement) =
      let u = self#get_unit_by_id id_unit in
      if Array.length fog > 0 then
        Fog.delete_unit_fog fog u#position u#vision_range;
      u#move (final_position (get_path p));
      if Array.length fog > 0 then
        Fog.add_unit_fog fog u#position u#vision_range

    method delete_building (id_building : Building.building_id) =
        let b = Hashtbl.find buildings id_building in
        if Array.length fog > 0 then
          Fog.delete_unit_fog fog b#position b#vision_range;
        Hashtbl.remove buildings id_building

    method get_value_resource = resource

    method has_resource amount =
      if resource < amount then false else true

    method use_resource amount =
      if resource < amount then
        raise Not_enough_resources
      else
        resource <- resource - amount

    method harvest_buildings_income =
      List.iter
        (fun b -> resource <- max 0 (resource + b#income))
        self#get_buildings

    method init (field: Battlefield.t) (players:logicPlayer list) =
        let (size_x,size_y) = Battlefield.size field in
        fog <- Array.make_matrix size_x size_y 0;
        List.iter
          (fun x -> Fog.add_unit_fog fog x#position x#vision_range)
          self#get_army

    method copy =
      let copy_hashtbl h =
        let h' = Hashtbl.create 97 in
        Hashtbl.iter (fun k v -> Hashtbl.add h' k (Oo.copy v)) h ;
        h'
      in
      let copy_matrix a =
        if a = [||] then [||]
        else begin
          let n = Array.length a
          and m = Array.length (a.(0)) in
          let a' = Array.make_matrix n m (a.(0).(0)) in
          for i = 0 to n - 1 do
            for j = 0 to m - 1 do
              a'.(i).(j) <- a.(i).(j)
            done
          done ;
          a'
        end
      in
      {<
          id_ = id_ ;
          army = copy_hashtbl army ;
          buildings = copy_hashtbl buildings ;
          resource = resource ;
          base = base ;
          fog = copy_matrix fog
      >}

    initializer
      match id with
      | None -> id_ <- Oo.id self;
      | _ -> ();

  end


class virtual player  ?(id) () =
  object (self)
  inherit logicPlayer ?id:id ()
  val mutable logic_player_list:logicPlayer list = []
  method virtual get_next_action :  Action.t

  method set_logic_player_list playerList = logic_player_list <- playerList
  method get_logic_player_list = logic_player_list
  method virtual update : (Types.update)  -> unit

end

type t = player

class dummy_player ?(id) (a: Action.t list) =
  object
    inherit player ?id:id ()
    val mutable actions = (a: Action.t list)
    method get_next_action  =
      if length a == 0 then
        ([], Action.End_turn)
      else
        let action= hd(actions) in
        actions<-tl(actions);
        action

    method update (u:Types.update) =
    ()

  end

let create_player () = new dummy_player []
let create_dummy_player actions = new dummy_player actions
