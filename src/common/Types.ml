(** @author Paul-Gallot Julien Grange et François Thiré *)


type error = Wrong_id_player

type id_player = int

(** Type of the data sent from the engine to the player/client *)
type update =
  | Game_over
  | You_win
  | Your_turn
  | Turn_of of id_player
  | Classement
  | Set_army of Unit.t list * id_player
  | Set_building of Building.t list * id_player
  | Add_unit of Unit.t * id_player
  | Add_building of Building.t * id_player
  | Delete_unit of Unit.unit_id *id_player (*Fog or kill*)
  | Delete_building of Building.building_id * id_player(*fog or kill*)
  | Move_unit of Unit.unit_id * Action.movement * id_player
  | Set_unit_hp of Unit.unit_id * int * id_player
  | Building_changed of Building.t
  | Set_unit_played of Unit.unit_id * id_player * bool
  | Harvest_income
  | Use_resource of int
(* for initialization only *)
  | Set_client_player of id_player
  | Set_logic_player_list of id_player list
  | Map of string
  | Config of string * string * string * string


let get_next_action_code = 0
let update_code = 1
let next_action_code = 2
let error_code = 3

let clock = 3.0




let disassemble str sep =
  let rec disassemble_aux str sep start lst =

    if String.contains_from str start sep
    then
      begin
	let index  = String.index_from str start sep in
	let length = index - start in
	disassemble_aux str sep (index+1) ((String.sub str start length)::lst)
      end
    else
      (String.sub str start (String.length str - start))::lst;
  in

  let lst = disassemble_aux str sep 0 [] in

  List.rev lst



let sep1 = '\000'
and sep2 = '\001'

let from_string (str : string) =

  let length = String.length str in

  (* exeption : ma_to_string(map) can contain any character *)
  

  if (length >= 3 && String.compare (String.sub str 0 3) "Map" == 0)
  then Map (String.sub str 3 (length - 3))
  else
    (* Not a map *)

    let lst = disassemble str sep1 in
    match lst with
      
    | [] -> failwith "Error in protocol"

    | t::q -> match t with

	      | "Game_over" -> Game_over

	      | "Your_turn" -> Your_turn

	      | "Turn_of" -> begin
			     match q with
			     | id_str::_ -> Turn_of (int_of_string id_str)
			     | _ -> failwith "Error in potocol"
			   end

	      | "Classement" -> Classement

	      | "You_win" -> You_win

	      | "Harvest_income" -> Harvest_income

	      | "Set_army" ->
                  begin match q with
                    | [units_str;id_str] ->
                        let unit_str_lst = disassemble units_str sep2 in
                        let unit_str_lst =
                          (* TODO this should probably be handled
                           * directly in disassemble *)
                          if unit_str_lst = [""] then [] else unit_str_lst
                        in
                          Set_army
                            (List.map
                               Config.config#unit_of_string
                               unit_str_lst,
                             int_of_string id_str)
                    | _ -> failwith "Error in protocol"
                  end

	      | "Set_building" ->
                  begin match q with
                    | [buildings_str;id_str] ->
                        let building_str_lst =
                          disassemble buildings_str sep2
                        in
                        let building_str_lst =
                          (* TODO this should probably be handled
                           * directly in disassemble *)
                          if building_str_lst = [""] then [] else
                            building_str_lst
                        in
                          Set_building
                            (List.map
                               Config.config#building_of_string
                               building_str_lst,
                             int_of_string id_str)
                    | _ -> failwith "Error in protocol"
                  end

	      | "Add_unit" -> begin
			      match q with
			      | unit_str::id_str::_ -> Add_unit (Config.config#unit_of_string unit_str, int_of_string id_str)
			      | _ -> failwith "Error in protocol"
			    end

	      | "Add_building" -> begin
				  match q with
				  | building_str::id_str::_ -> Add_building (Config.config#building_of_string building_str, int_of_string id_str)
				  | _ -> failwith "Error in protocol"
				end


	      | "Delete_unit" -> begin
				 match q with
				 | unit_id_str::id_str::_ -> Delete_unit (int_of_string unit_id_str, int_of_string id_str)
				 | _ -> failwith "Error in protocol"
			       end


	      | "Delete_building" -> begin
				     match q with
				     | building_id_str::id_str::_ -> Delete_building (int_of_string building_id_str, int_of_string id_str)
				     | _ -> failwith "Error in protocol"
				   end


	      | "Move_unit" -> begin
			       match q with
			       | unit_id_str::mov_str::id_str::_ -> Move_unit (int_of_string unit_id_str, Action.mov_from_string mov_str, int_of_string id_str)
			       | _ -> failwith "Error in protocol"
			     end


	      | "Set_unit_hp" -> begin
				 match q with
				 | unit_id_str::int_str::id_str::_ -> Set_unit_hp (int_of_string unit_id_str, int_of_string int_str, int_of_string id_str)
				 | _ -> failwith "Error in protocol"
			       end

	      | "Set_client_player" -> begin
				       match q with
				       | id_str::_ -> Set_client_player (int_of_string id_str)
				       | _ -> failwith "Error in protocol"
				     end


	      | "Set_logic_player_list" -> begin
					   match q with
					   | id_lst_str::_ -> let id_str_lst = disassemble id_lst_str sep2 in
							      Set_logic_player_list (List.map int_of_string id_str_lst)
					   | _ -> failwith "Error in protocol"
					 end

	      | "Building_changed" -> begin
				      match q with
				      | building_str::_ -> Building_changed (Config.config#building_of_string building_str)
				      | _ -> failwith "Error in protocol"
				    end

	      | "Config" -> begin
			    match q with
			    | s1::s2::s3::s4::_ -> Config (s1,s2,s3,s4)
			    | _ -> failwith "Error in protocol"
			  end

	      | _ -> failwith "Error in protocol"



let to_string update = 
  let str1 = String.make 1 sep1
  and str2 = String.make 1 sep2 in
  match update with

  | Game_over ->
     "Game_over"
       
  | Your_turn ->
     "Your_turn"

  | Turn_of id_player ->
     String.concat str1 ("Turn_of"::(string_of_int id_player)::[])

  | Classement ->
     "Classement"

  | You_win ->
     "You_win"

  | Harvest_income ->
     "Harvest_income"

  | Set_army (unit_list, id_player) ->
     let str = String.concat str2 (List.map Config.config#string_of_unit unit_list) in
     String.concat str1 ("Set_army"::str::(string_of_int id_player)::[])

  | Set_building (building_list, id_player) ->
     let str = String.concat str2 (List.map Config.config#string_of_building building_list) in
     String.concat str1 ("Set_building"::str::(string_of_int id_player)::[])

  | Add_unit (unit, id_player) ->
     String.concat str1 ("Add_unit"::(Config.config#string_of_unit unit)::(string_of_int id_player)::[])

  | Add_building (building, id_player) ->
       String.concat str1 ("Add_building"::(Config.config#string_of_building building)::(string_of_int id_player)::[])

  | Delete_unit (id_unit, id_player) ->
     String.concat str1 ("Delete_unit"::(string_of_int id_unit)::(string_of_int id_player)::[])

  | Delete_building (id_building, id_player) ->
     String.concat str1 ("Delete_building"::(string_of_int id_building)::(string_of_int id_player)::[])

  | Move_unit (id_unit, movement, id_player) ->
     String.concat str1 ("Move_unit"::(string_of_int id_unit)::(Action.mov_to_string movement)::(string_of_int id_player)::[])
		   
  | Set_unit_hp (id_unit, int, id_player) ->
     String.concat str1 ("Set_unit_hp"::(string_of_int id_unit)::(string_of_int int)::(string_of_int id_player)::[])
		   
  | Set_client_player id_player ->
     String.concat str1 ("Set_client_player"::(string_of_int id_player)::[])
		   
  | Set_logic_player_list id_player_list ->
     let str = String.concat str2 (List.map string_of_int id_player_list) in
     String.concat str1 ("Set_logic_player_list"::str::[])
		   
  | Map string ->
     String.concat "" ("Map"::string::[])
		   
  | Building_changed building ->
     String.concat str1 ("Building_changed"::Config.config#string_of_building building::[])
		   
  | Config (s1,s2,s3,s4) ->
     String.concat str1 ("Config"::s1::s2::s3::s4::[])
		   
  | _ -> failwith "Update not known"
