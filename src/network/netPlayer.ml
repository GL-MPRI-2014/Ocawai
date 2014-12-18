open Unix
open Marshal
open Types
open Send_recv

module Log = Log.Make (struct let section = "NetPlayer" end)

class netPlayer ?(id) (s : file_descr) = 
object (self)
  inherit Player.player ?id:id

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

(*@@@@@@@@@@@@@@@@@@@@ new @@@@@@@@@@@@@@@@@@@@
let boolean = Send_recv.send sockfd 0 (to_string [Closures]) 3.0

With
 -> sockfd the socket
 -> 0 the code for "Get_next_action"
 -> to_string [Closures]
 -> 3.0 is the timeout

if boolean is false then kill this player 
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)

(*#################### old ####################*)
    to_channel out_channel (Get_next_action) [Closures];
(*#############################################*)

    flush out_channel ;
    Log.infof "#get_next_action sent" ;

(*@@@@@@@@@@@@@@@@@@@@ new @@@@@@@@@@@@@@@@@@@@
let receipt  = Send_recv.recv sockfd 3.0

With
 -> sockfd the socket
 -> 3.0 is the timeout
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)

(*#################### old ####################*)
    let s = (from_channel in_channel : receive) in
(*#############################################*)

    Log.infof "#get_next_action received" ;

(*@@@@@@@@@@@@@@@@@@@@ new @@@@@@@@@@@@@@@@@@@@
match receipt with
 | Some(2, _) -> self#manage_gna
 | Some(3, _) -> [Position.create (0,0)], Action.Wait
 | None -> kill this player

With
 -> '2' is the code for "Next_action"
 -> '3' is the code for "Error"
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)

(*#################### old ####################*)
    match s with 
    | Next_action a -> a
    | Error _ -> [Position.create (0,0)], Action.Wait (* By default Wait *)
(*#############################################*) 

  (* send updates over the network *)
		   
  method update u =
    to_channel out_channel (Update u) [Closures];

end
