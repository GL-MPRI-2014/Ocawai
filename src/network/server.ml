let port =
  if Array.length Sys.argv = 2 then
    int_of_string Sys.argv.(1)
  else 
    failwith "not specified port"

let fd_listen = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let () =
  try
    Unix.setsockopt fd_listen Unix.SO_REUSEADDR true;
    Unix.bind fd_listen (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen fd_listen 16;
    Unix.set_nonblock fd_listen
  with
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
        failwith "port already in use"
      
module FDSet = Set.Make
  (struct
    type t = Unix.file_descr
    let compare = compare
   end
  )
  
let clients = ref (FDSet.singleton fd_listen)
  
let input_buffer = Hashtbl.create 0
let output_buffer = Hashtbl.create 0
let flush_buffer = Hashtbl.create 0
  
let buffer_size = 1024
let buffer = String.make buffer_size ' '
  
let handle client requests =
  List.iter
    (fun request ->
      let data = Printf.sprintf "Bien recu : \"%s\"\n" request in
      Printf.fprintf stdout "%s" request ;
      flush stdout ;
      Hashtbl.replace output_buffer client
	(try Hashtbl.find output_buffer client ^ data
         with Not_found -> data
	);
    ) requests

let () =
  while true do
    let (fd_read, _, _) =
      Unix.select (FDSet.elements !clients) [] [] 1.0 in
    List.iter
      (fun client ->
        if client = fd_listen then
          begin
            let (client, addr) = Unix.accept fd_listen in
            clients := FDSet.add client !clients;
            Unix.set_nonblock client
          end
        else
          begin
            let read_size =
              try
                Some (Unix.read client buffer 0 buffer_size)
              with Unix.Unix_error (_, _, _) ->
                None in
            match read_size with
              | None | Some 0 ->
                Hashtbl.remove input_buffer client;
                Hashtbl.remove output_buffer client;
                Hashtbl.remove flush_buffer client;
                
                clients := FDSet.remove client !clients;
                Unix.close client

              | Some read_size ->
                let data = String.sub buffer 0 read_size in
                Hashtbl.replace input_buffer client
                  (try Hashtbl.find input_buffer client ^ data
                   with Not_found -> data);
                try
                  while true do
                    let data = Hashtbl.find input_buffer client in
                    let index = String.index data '#' in
                    Hashtbl.replace input_buffer client
		      (String.sub data
			 (index + 1)
                         (String.length data - index - 1)
		      );
                    Hashtbl.replace flush_buffer client
		      ((try Hashtbl.find flush_buffer client
			with Not_found -> []
		       ) @ [String.sub data 0 index]
		      )
                  done
                with Not_found -> ()
          end
      ) fd_read;
    
    Hashtbl.iter handle flush_buffer;
    Hashtbl.clear flush_buffer;
    
    let (_, fd_write, _) =
      Unix.select [] (FDSet.elements !clients) [] 1.0 in
    let fd_write =
      List.filter (Hashtbl.mem output_buffer) fd_write in
    List.iter
      (fun client ->
        let data = Hashtbl.find output_buffer client in
        let write_size =
          try
            Some (Unix.single_write client data 0 (String.length data))
          with
            | Unix.Unix_error (Unix.EAGAIN, _, _) ->
              Some 0
            | Unix.Unix_error (_, _, _) ->
              None in
	
        match write_size with
          | None ->
            Hashtbl.remove input_buffer client;
            Hashtbl.remove output_buffer client;
            Hashtbl.remove flush_buffer client;
	    
            clients := FDSet.remove client !clients;
            Unix.close client
          | Some write_size ->
            if write_size = String.length data then
	      Hashtbl.remove output_buffer client
            else
	      Hashtbl.replace output_buffer client
		(String.sub data
		   write_size
                   (String.length data - write_size)
		)
      ) fd_write;
  done
