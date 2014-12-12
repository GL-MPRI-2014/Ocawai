let (port, n) =
  if Array.length Sys.argv = 3 then
    let port = int_of_string Sys.argv.(1) in
    let n = int_of_string Sys.argv.(2) in
    (port, n) 
  else 
    failwith "Usage :: ./test port n"
      
      
let _ = 
  let list = Network_tool.open_n_connections port n in
  Printf.printf "Receive %d connection(s)\n" (List.length list)
   
