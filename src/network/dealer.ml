open Unix
open Marshal
open Types

module Log = Log.Make (struct let section = "Dealer" end)

class dealer (s : file_descr) =
object (self)

  (* The socket we read from and write into to communicate with the net player over the network *)
  val mutable sockfd = s

  (* in_channel and out_channel corresponding to that socket *)
  val mutable in_channel = in_channel_of_descr s
  val mutable out_channel = out_channel_of_descr s

  (* list of all the other (logical) players *)
  val mutable logicPlayerList = []

  (* my client player *)
  val mutable clientPlayer : Player.player option = Some (Player.create_dummy_player [])


  (* for initialization *)

  method set_logicPlayerList id_list =
    let rec create_list logp_list = function
      | [] -> logp_list
      | t::q -> create_list ((new Player.logicPlayer ?id:(Some t) ())::logp_list) q
    in
    let logp_list = create_list [] id_list in
    logicPlayerList <- logp_list

  method set_player_id id = ()



  (* Mazzocchi asked for it *)

  method change_socket s =
    sockfd <- s;
    in_channel <- in_channel_of_descr s;
    out_channel <- out_channel_of_descr s




  (* scans tab_players and retreives the player with id id *)

  method list_scan id =
    let rec fonction_a_la_con id = function
      | [] -> raise Not_found
      | t::q when t#get_id = id -> t
      | _::q -> fonction_a_la_con id q
    in
    fonction_a_la_con id logicPlayerList


  (* verifies that clientPlayer has been set *)

  method is_set =
    match clientPlayer with
    | None -> failwith "Player not set"
    | Some clip -> clip


  (* gives the player which id is id - or fails *)

  method get_player id =
    let clip = self#is_set in
    if clip#get_id = id then (clip :> Player.logicPlayer) else
      try
	self#list_scan id
      with
	(*TO DO : add a raise exception *)

	Not_found -> failwith "Wrong id"
  (* TODO *)
  method set_map str =
    ()


  (* manages Get_next_action *)

  method manage_gna =
    let clip = self#is_set in
    let action = clip#get_next_action in
    Log.info "send \"next_action\"" ;
    let success  = Send_recv.send sockfd Types.next_action_code (Action.to_string action) Types.clock in
    
    if not success then
      failwith "send \"next_action failure in dealer !"

  (* do what has to be done with an update... *)
	
  method manage_update = function
    | Game_over -> ()
    | Your_turn -> ()
    | Turn_of _ -> ()
    | You_win -> ()
    | Harvest_income -> ()
    | Classement -> ()
    | Set_unit_played _ -> ()
    | Use_resource _ -> ()
    | Set_army (l,id) -> (self#get_player id)#set_army l
    | Set_building (l,id) -> (self#get_player id)#set_buildings l
    | Add_unit (u,id) -> (self#get_player id)#add_unit u
    | Add_building (b,id) -> (self#get_player id)#add_building b
    | Delete_unit (u,id) -> (self#get_player id)#delete_unit u
    | Delete_building (b,id) -> (self#get_player id)#delete_building b
    | Move_unit (u,p,id) -> (self#get_player id)#move_unit u p
    | Set_unit_hp (u,h,id) -> (self#get_player id)#set_unit_hp u h
    (* for initialization only *)
    | Set_client_player id -> self#set_player_id id
    | Set_logic_player_list lst -> self#set_logicPlayerList lst
    | Map str -> self#set_map str
    | Building_changed b -> ()
    | Config (s1,s2,s3,s4) -> Config.config#load_from_strings s1 s2 s3 s4

  (* please give a call to this method just after having created this object *)

  method run = 
    while true 
    do 
      Log.info "recv" ;
      let receipt  = Send_recv.recv sockfd Types.clock in

      match receipt with
	| Some (code, _) when code = Types.get_next_action_code ->
	  Log.info "-> \"get_next_action\"";
	  self#manage_gna 
	| Some(code, update) when code = Types.update_code ->
	  Log.info "-> \"update\"";
	  self#manage_update (Types.from_string update)
	| None -> Log.info "-> None"
	| Some (n,_) -> Log.info "-> code : %d" n
    done
end

type t = dealer
	   
let create_dealer sockfd = new dealer sockfd
