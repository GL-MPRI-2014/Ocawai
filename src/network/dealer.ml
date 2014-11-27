open Unix
open Marshal
open Types

class dealer (s : file_descr) (clip : ClientPlayer.client_player) =
object (self)
	 
  (* The socket we read from and write into to communicate with the net player over the network *)
  val mutable sockfd = s

  (* in_channel and out_channel corresponding to that socket *)
  val mutable in_channel = in_channel_of_descr s
  val mutable out_channel = out_channel_of_descr s

  (* list of all the other (logical) players *)
  val mutable logicPlayerList = []

  (* my client player *)
  val cli_player = clip
		


  method set_logicPlayerList lpl = 
    logicPlayerList <- lpl

  method get_logicPlayerList = 
    logicPlayerList


  (* Mazzocchi asked for it *)
	
  method change_socket s =
    sockfd <- s;
    in_channel <- in_channel_of_descr s;
    out_channel <- out_channel_of_descr s




  (* scan tab_players and retreive the player with id id *)

  method list_scan id = 
    let rec fonction_a_la_con id = function
      | [] -> raise Not_found
      | t::q when t#get_id = id -> t 
      | _::q -> fonction_a_la_con id q
    in
    fonction_a_la_con id logicPlayerList

  (* give the player which id is id - or send Wrong_id_player and give cli_player *)


  method get_player id =
    if cli_player#get_id = id
    then cli_player
    else
      try
	self#list_scan id
      with
	Not_found -> (to_channel out_channel (Error Wrong_id_player) [Closures]; cli_player)
		     
		     


  (* manages Get_next_action *)

  method manage_gna = 
    let action = cli_player#get_next_action in
    to_channel out_channel (Next_action action) [Closures]
    

  (* do what has to be done with an update... *)

  method manage_update = function
    | Game_over -> ()
    | Classement -> ()
    | Set_army (l,id) -> (self#get_player id)#set_army l
    | Set_building (l,id) -> (self#get_player id)#set_buildings l
    | Add_unit (u,id) -> (self#get_player id)#add_unit u
    | Add_building (b,id) -> (self#get_player id)#add_building b
    | Delete_unit (u,id) -> (self#get_player id)#delete_unit u
    | Delete_building (b,id) -> (self#get_player id)#delete_building b
    | Move_unit (u,p,id) -> (self#get_player id)#move_unit u p



  (* please give a call to this method just after having created this object *)

  method run = 
    while true 
    do 
      let m = (from_channel in_channel : send) in
      match m with
      | Get_next_action -> self#manage_gna
      | Update update -> self#manage_update update
    done
      

end

type t = dealer

let create_dealer sockfd tablogp clip = new dealer sockfd tablogp clip

