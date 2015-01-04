open Unix
open Marshal
open Types
open Send_recv

module Log = Log.Make (struct let section = "NetPlayer" end)

class netPlayer ?(id) (s : file_descr) =
object (self)
  inherit Player.player ?id:id ()

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
    Log.infof "send \"get_next_action\"" ;
    let success = Send_recv.send sockfd Types.get_next_action_code "" Types.clock in
    
    if not success then
      failwith "send \"get_next_action\" failure in netPlayer !"
    else
      begin
	Log.infof "recv";
	let receipt  = Send_recv.recv sockfd Types.clock in 

	match receipt with
	| Some(code, str) when code = Types.next_action_code -> 
	  Log.infof "-> \"next_action\"";
	  Action.from_string str
	| None ->
	  Log.infof "-> None";
	  failwith "kill the player in netPlayer !"
	  (* [Position.create (0,0)], Action.Wait *)
	| _ ->
	  failwith "Error fatal in netPlayer !"
	  (* [Position.create (0,0)], Action.Wait *)
      end

	  
  method set_logicPlayerList playersList =
	()

  method get_logicPlayerList =
	logicPlayerList

  (* send updates over the network *)

  method update u =
    Log.infof "send  \"update\"";
    
    let success = Send_recv.send sockfd Types.update_code (Types.to_string u) Types.clock in

    if not success then
      failwith "send \"update\" failure in netPlayer !"
    
end
