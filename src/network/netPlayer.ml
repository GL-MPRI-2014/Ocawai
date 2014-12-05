open Unix
open Marshal
open Types

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
    to_channel out_channel (Get_next_action) [Closures];
    let s = (from_channel in_channel : receive) in
    match s with 
    | Next_action a -> a
    | Error _ -> [Position.create (0,0)], Action.Wait (* By default Wait *) 

  method set_logicPlayerList playersList =
	()

  method get_logicPlayerList =
	logicPlayerList

  (* send updates over the network *)
		   
  method update u =
    to_channel out_channel (Update u) [Closures];

end
