open Unix
open Marshal
open Types

class netPlayer (sockfd : file_descr) = 
object (self)
  inherit Player.player

  (* The socket we read from and write into to communicate with the dealer over the network *)
  val mutable sockfd = sockfd

  (* in_channel and out_channel corresponding to that socket *)
  val in_channel = in_channel_of_descr sockfd
  val out_channel = out_channel_of_descr sockfd
		     



  (* Mazzocchi asked for it *)
		     
  method change_socket s =
    sockfd = s
	       

  (* asks for the next action over the network
     does not handle timeout yet *)

  method get_next_action = 
    to_channel out_channel (Get_next_action) [Closures];
    let s = (from_channel in_channel : reveive) in
    match s with 
    | Next_action a -> a
    | Error _ -> () (* What could be the problem ? *) 


  (* send updates over the network *)
		   
  method update u =
    to_channel out_channel u [Closures];

end

type t = netPlayer

let create_netPlayer sockfd = new netPlayer sockfd 
