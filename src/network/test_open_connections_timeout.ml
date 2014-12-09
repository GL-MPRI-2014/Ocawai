let (port, timeout) =
  if Array.length Sys.argv = 3 then
    let port = int_of_string Sys.argv.(1) in
    let timeout = float_of_string Sys.argv.(2) in
    (port, timeout) 
  else 
    failwith "Usage :: ./test port timeout"
      
      
let _ = 
  let list = Network_tool.open_connections_timeout port timeout in
  Printf.printf "Receive %d connection(s)\n" (List.length list)
   
