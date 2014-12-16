open Unix
open Marshal
open Types
open Send_recv

module Log = Log.Make (struct let section = "NetPlayer" end)

class netPlayer (s : file_descr) (a:Unit.t list) (b:Building.t list) = 
object (self)
  inherit Player.player a b 

  (* The socket we read from and write into to communicate with the dealer over the network *)
  val mutable sockfd = s
  val mutable logicPlayerList = [] 
  (* in_channel and out_channel corresponding to that socket *)
  val mutable in_channel = in_channel_of_descr s
  val mutable out_channel = out_channel_of_descr s
		     
  (* Mazzocchi asked for it *)
		     
  method change_socket s =
    sockfd <- s;
    in_channel <- in_channel_of_descr s;
    out_channel <- out_channel_of_descr s	       

  (* asks for the next action over the network
     does not handle timeout yet *)

  method get_next_action = 
    Log.infof "#get_next_action" ;

    let boolean = Send_recv.send sockfd 0 "" 3.0 in
    (* TODO: if boolean is false then kill this player*)
    flush out_channel ;
    
    if not boolean
    then () (* kill the player *)
    else
      Log.infof "#get_next_action sent" ;
    
    let receipt  = Send_recv.recv sockfd 3.0 in 
    Log.infof "#get_next_action received";
    match receipt with
    | Some(2, str) -> Action.from_string str
    | None -> [Position.create (0,0)], Action.Wait (* kill this player *)
    | _ -> [Position.create (0,0)], Action.Wait (* Wait by default *)
		       
  method set_logicPlayerList playersList =
	()

  method get_logicPlayerList =
	logicPlayerList

  (* send updates over the network *)
	  
  method update u =
    Log.infof "#update";
    
    let boolean = Send_recv.send sockfd 1 (Types.to_string u) 3.0 in
    flush out_channel ;
    if not boolean
    then ()    (* TODO: kill this player*)
    else
      Log.infof "#update sent"
    
end
