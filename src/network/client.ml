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


let ip = Sys.argv.(1)
let port = int_of_string Sys.argv.(2)

let () = 
  let sock = open_connection ip port in
  Unix.close sock
