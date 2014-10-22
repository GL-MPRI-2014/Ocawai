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




module FDSet = Set.Make struct
  type t = file_descr
  let compare = compare
end

let clients = ref (FDSet.singleton listen_sock)

let _ =
  while true do
    let (fd_read, _, _) = select (FDSet.elements !clients) [] [] -1 in
    List.iter (fun client ->
      begin
	if client = listen_sock then
	  let (client, _) = accept listen_sock in
	  clients := FDSet.add client !clients
	else
	  begin
	    let count = read client buffer 0 buffer_size) in
            if count = 0 then
	      begin
		clients := FDSet.remove client !clients ;
		close client
	      end
	    else
	      Printf.printf "%s\n" buffer
          end
      end
    ) fd_read
  done 
