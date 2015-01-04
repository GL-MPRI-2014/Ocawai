(**
 * Given a new architecture, the game does not use server
 * for strictly speaking. This module provides the necessary
 * tools to handling socket and implements the connexion
 * initialization functions.
 *
 * @author Mazzocchi Nicolas
 *)


(* SERVER *)


(**
 * This function create a listening socket
 * @param port Listening port
 * @return Listening socket
 *)
let create_listener port = 
  let fd_listen = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd_listen Unix.SO_REUSEADDR true;
  try
    Unix.bind fd_listen (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen fd_listen 16;
    (*Unix.set_nonblock fd_listen;*)
    fd_listen
  with
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Unix.close fd_listen ;
        failwith "The server port already in use : give another port to start the server"
    | Unix.Unix_error (_, "bind", _) ->
      Unix.close fd_listen ;
         failwith "The server port illegal : give another port to start the server"
	
(**
 * This function initializes the connection with
 * list of clients connected sockets
 * @param port Listening port
 * @param n Expected number of clients
 * @return List of clients socket
 *)
let open_n_connections port n =
  let fd_listen = create_listener port in
  let rec open_n_connections' n list =
    if n = 0 then
      list
    else
      begin
	let (client, _) = Unix.accept fd_listen in
	open_n_connections' (n-1) (client::list)
      end
  in
  let list = open_n_connections' n [] in
  Unix.close (fd_listen);
  list


(**
 * This function is the same as the previous one,
 * but accepting all clients during time.
 * @param port Listening port
 * @param timeout Waiting time
 * @return List of clients socket
 *)
let open_connections_timeout port timeout =
  let fd_listen = create_listener port in
  let off_time = Sys.time () +. timeout in
  let rec open_connections_timeout' list =
    let timeout = off_time -.Sys.time () in
    let (fd_read, _, _) = Unix.select [fd_listen] [] [] timeout in
    if fd_read = [] then
      list
    else
      let (client, _) = Unix.accept fd_listen in
      open_connections_timeout' (client::list)
  in
  let list = open_connections_timeout' [] in
  Unix.close (fd_listen);
  list

(*
exception Break
let break s = raise Break


let open_connexions_alarm port timeout =
  let fd_listen = create_listener port in
  let handle = Sys.signal Sys.sigalrm (Sys.Signal_handle break) in
  let _ = Unix.alarm timeout in
  let rec open_connexions_alarm' list =
    try 
      begin
	let (_, _, _) = Unix.select [fd_listen] [] [] (-1.) in
	let (client, _) = Unix.accept fd_listen in
        open_connexions_alarm' (client::list)
      end
    with Break ->
      list
  in
  let list = open_connexions_alarm' [] in
  let _ = Sys.signal Sys.sigalrm handle in
  Unix.close (fd_listen);
  list
*)


(* CLIENT *)


(**
 * This function creates a connection to the server
 * @param port Server port
 * @param ip Server address
 * @return Socket
**)
let open_connection ip port =
  let addr = Unix.inet_addr_of_string ip in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sock_addr = Unix.ADDR_INET (addr, port) in
  try 
    Unix.connect sock sock_addr;
    sock
  with 
    | Unix.Unix_error (_, "connect", _) -> 
      Unix.close sock;
      failwith "The server connection attempt failed : Check the address and listening port"


(* COMUNNICATION *)



(**
 * This function is blocking off version of [read].
 * @param timeout Waiting time
 * The other parameters are those of [read]
 * @return Option of read caractere number
 *)
let read_timeout sock buffer buffer_size timeout =
  if timeout = 0. then
    None
  else 
    begin
      let (fd_read, _, _) = Unix.select [sock] [] [] timeout in
      if fd_read = [] then
	None
      else
	  Some(Unix.read sock buffer 0 buffer_size);
    end


(**
 * This function is blocking off version of [write].
 * @param timeout Waiting time
 * The other parameters are those of [write]
 * @return Option of write caractere number
 *)
let write_timeout sock buffer buffer_size timeout =
  if timeout = 0. then
    None
  else
    begin
      let (_, fd_write, _) = Unix.select [] [sock] [] timeout in
      if fd_write = [] then
	None
      else
	Some (Unix.write sock buffer 0 buffer_size)
    end
