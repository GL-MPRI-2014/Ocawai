open Unix
open Marshal
open Types

class dealer (sockfd : file_descr) (tablogp : LogP.t list) (clip : CliP.t) =
object (self)
	 
  (* The socket we read from and write into to communicate with the net player over the network *)
  val mutable sockfd = sockfd

  (* in_channel and out_channel corresponding to that socket *)
  val in_channel = in_channel_of_descr sockfd
  val out_channel = out_channel_of_descr sockfd

  (* list of all the other (logical) players *)
  val tab_players = tablogp

  (* my client player *)
  val cli_player = clip
		



  (* Mazzocchi asked for it *)
	
  method change_socket s =
    sockfd = s




  (* scan tab_players and retreive the player with id id *)

  method list_scan id = 
    let rec fonction_a_la_con id = function
      | [] -> raise Not_found
      | t::q when t#get_id = id -> t 
      | _::q -> fonction_a_la_con id q
    in
    fonction_a_la_con id tab_players

  (* give the player which id is id - or send Wrong_id_player *)

  method get_player id =
    if cli_player#get_id = id
    then cli_player
    else
      try
	list_scan id
      with
	Not_found -> to_channel out_channel (Error Wrong_id_player) [Closures];


  (* manages Get_next_action*)

  method manage_gna = 
    let action = cli_player#get_next_action in
    to_channel out_channel (Next_action action)
    

  (* do what have to be done with an update... *)

  method manage_update = function
    | Game_over -> ()
    | Classement -> ()
    | Set_army (l,id) -> (get_player id)#set_army l
    | Set_building (l,id) -> (get_player id)#set_building l
    | Add_unit (u,id) -> (get_player id)#add_unit u
    | Add_building (b,id) -> (get_player id)#add_building b
    | Delete_unit (u,id) -> (get_player id)#delete_unit u
    | Delete_building (b,id) -> (get_player id)#delete_building b
    | Move_unit (u,p,id) -> (get_player id)#move_unit u p



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

type t = cliPlayer

let create_cliPlayer sockfd = new cliPlayer sockfd;

