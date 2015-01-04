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

   (*
    let chars = Network_tool.write_timeout sockfd "titi" 0 4 1. in
match chars with 
| Some (n) -> Log.infof "nb chars %d" n;failwith "end"
| None ->  Log.infof "None";failwith "end"
   *)

 let success = Send_recv.send sockfd Types.get_next_action_code "" 3.0 in
    (* TODO: if boolean is false then kill this player*)
    Log.infof "Sent";
    flush out_channel ;

    if not success then
      failwith "kill the player in netPlayer !" (* kill the player *)
    else
      begin
	Log.infof "#get_next_action sent" ;
	
	let receipt  = Send_recv.recv sockfd 3.0 in 
	Log.infof "#get_next_action received";
	flush out_channel ;
	match receipt with
	| Some(code, str) when code = Types.next_action_code -> 
	  Log.infof "#next_action";
	  flush out_channel;
	  Action.from_string str
	| None ->
	  Log.infof "#None";
	  flush out_channel;
	  [Position.create (0,0)], Action.Wait (* kill this player *)
	| _ ->
	  Log.infof "#Error Fatal";
	  flush out_channel;
	  [Position.create (0,0)], Action.Wait (* Wait by default *)
      end

	  
  method set_logicPlayerList playersList =
	()

  method get_logicPlayerList =
	logicPlayerList

  (* send updates over the network *)
	  
  method update u =
    Log.infof "#update";
    
    let success = Send_recv.send sockfd Types.update_code (Types.to_string u) 3.0 in
    flush out_channel ;
    if not success then
      failwith "kill the player in netPlayer !"
    else
      Log.infof "#update sent"
    
end
