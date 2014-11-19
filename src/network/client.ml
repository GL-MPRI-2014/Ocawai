(**
 * Implement this modulates the client. It is not yet define,
 * how the client will be used. Thus, once connected it will
 * connect immediately.
 *
 * @author Mazzocchi Nicolas
 *)


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
      failwith "connexion fail"


(**
 * Server address is passed as a parameter 
**)
let ip = Sys.argv.(1)


(**
 * Server port is passed as a parameter 
**)
let port = int_of_string Sys.argv.(2)


(**
 * The mais function of client
**)
let () = 
  let sock = open_connection ip port in
  Unix.close sock
