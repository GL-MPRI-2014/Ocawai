#load "Unix.cma"
open Unix

let _ =
  let port = 1024 in
  let listen_sock = Unix.socket PF_INET SOCK_STREAM 0 in
  bind listen_sock (ADDR_INET  (inet_addr_any,port)) ;
  listen listen_sock 16 ;
  let (sock,_) = accept listen_sock in
  close listen_sock ;
  let count = send sock "coucou" 0 (String.length "coucou") [] in
  close sock ;
  count

