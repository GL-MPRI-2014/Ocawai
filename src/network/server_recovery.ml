(**
 * This module implement the future recovery connection server.
 * When a client has disconnected unfortunately, it will send a
 * key to this server for come back in the game.
 *
 * @author Mazzocchi Nicolas
**)


(**
 * This is the listening port
**)
let port =
  if Array.length Sys.argv = 2 then
    int_of_string Sys.argv.(1)
  else 
    failwith "not specified port"


(**
 * This is the lisening socket
**)
let fd_listen = 
  let fd_listen = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd_listen Unix.SO_REUSEADDR true;
  try
    Unix.bind fd_listen (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen fd_listen 16 ;
    Unix.set_nonblock fd_listen ;
    fd_listen
  with
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Unix.close fd_listen ;
        failwith "port already in use"


(**
 * This is the structure of the set of client
**)
module FDSet = Set.Make
  (struct
    type t = Unix.file_descr
    let compare = compare
   end
  )

(**
 * This is the set of client
**)
let clients = ref (FDSet.singleton fd_listen)


(**
 * This is the structure of incomplete requests
**)
let input_buffer = Hashtbl.create 0


(**
 * This is the size of buffer
**)
let buffer_size = 1024


(**
 * This is the reading buffer
**)
let buffer = String.make buffer_size ' '


(**
 * This is the list of accepting keys 
**)
let keys  = "titi"::"toto"::[]
  

(**
 * This function of connecting clients 
**)
let connection fd_listen =
  let (client, _) = Unix.accept fd_listen in
  clients := FDSet.add client !clients ;
  Unix.set_nonblock client


(**
 * This function of disconnecting client in this server
**)
let disconnection client =
  Hashtbl.remove input_buffer client ;
  clients := FDSet.remove client !clients


(**
 * This function that accepts or rejecte clients
**)
let handle client request =
  disconnection client ;
  if List.mem request keys then
    begin
      Printf.fprintf stdout "%s accepted" request ;
      flush stdout ;
    (* swap socket ; dont close *)
      Unix.close client
    end
  else
    begin
      Printf.fprintf stdout "%s refused" request ;
      flush stdout ;
      Unix.close client
    end


(**
 * This function extract the client requests
**)
let read_scan client =
      let read_size =
        try
          Some (Unix.read client buffer 0 buffer_size)
        with
	  Unix.Unix_error (_, _, _) -> None
      in
      match read_size with
        | None | Some 0 ->
	  disconnection client ;
	  Unix.close client

        | Some read_size ->
          let new_data = String.sub buffer 0 read_size in
	  let data = 
	    try
	      Hashtbl.find input_buffer client ^ new_data
	    with
		Not_found -> new_data
	  in	    
          Hashtbl.replace input_buffer client data ;

          try
            let index = String.index data '#' in	    
	    handle client (String.sub data 0 index)
	  with
	      Not_found -> ()


(**
 * This function divides the treatment of listening socket and clients.
**)
let reading client =
  if client = fd_listen then
    connection client
  else
    read_scan client


(**
 * This function performs a reading scan
**)
let read_scan () =
  while true do
    let (fd_read, _, _) = Unix.select (FDSet.elements !clients) [] [] 1.0 in
    List.iter reading fd_read
  done
