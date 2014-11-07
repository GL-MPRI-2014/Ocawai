(**
 * Given a new architecture, the game does not use server
 * for strictly speaking. This module provides the necessary
 * tools to handling socket and implements the connexion
 * initialization functions.
 *
 * @author Mazzocchi Nicolas
 *)


(**
 * This function create a listening socket
 * @param port Listening port
 * @return Listening socket
 *)
let create_listener port = 
  let fd_listen = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    Unix.setsockopt fd_listen Unix.SO_REUSEADDR true;
    Unix.bind fd_listen (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen fd_listen 16;
    (*Unix.set_nonblock fd_listen;*)
    fd_listen
  with
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
        failwith "port already in use"

(**
 * This function initializes the connection with
 * list of clients connected sockets
 * @param port Listening port
 * @param n Expected number of clients
 * @return List of clients socket
 *)
(*
let connexions port n =
  let fd_listen = create_listener port in
  let rec connexions' n list =
    if n = 0 then
      list
    else
      begin
	let (client, _) = Unix.accept fd_listen in
	connexions' (n-1) (client::list)
      end
  in
  let list = connexions' n [] in
  Unix.close (fd_listen);
  list
*)

exception Break
let break s = raise Break


(**
 * This function is the same as the previous one,
 * but accepting all clients during time.
 * @param port Listening port
 * @param timeout Waiting time
 * @return List of clients socket
 *)
let connexions port timeout =
  let fd_listen = create_listener port in
  let handle = Sys.signal Sys.sigalrm (Sys.Signal_handle break) in
  let _ = Unix.alarm timeout in
  let rec connexions' list =
    try 
      begin
	let (_, _, _) = Unix.select [fd_listen] [] [] (-1.) in
	let (client, _) = Unix.accept fd_listen in
	connexions' (client::list)
      end
    with Break ->
      list
  in
  let list = connexions' [] in
  let _ = Sys.signal Sys.sigalrm handle in
  Unix.close (fd_listen);
  list


(**
 * This function is blocking off version of [read].
 * @param timeout Waiting time
 * The other parameters are those of [read]
 * @return Option of read caractere number
 *)
let read_timeout sock buffer flags buffer_size timeout =
  let (fd_read, _, _) = Unix.select [sock] [] [] timeout in
  if fd_read = [] then
    None
  else 
    Some (Unix.read sock buffer flags buffer_size)


(**
 * This function is blocking off version of [write].
 * @param timeout Waiting time
 * The other parameters are those of [write]
 * @return Option of write caractere number
 *)
let write_timeout sock buffer flags buffer_size timeout =
  let (_, fd_write, _) = Unix.select [] [sock] [] timeout in
  if fd_write = [] then
    None
  else 
    Some (Unix.single_write sock buffer flags buffer_size)
