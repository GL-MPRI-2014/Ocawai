
let (ip, port) =
  if Array.length Sys.argv = 3 then
    let ip = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    (ip, port) 
  else 
    failwith "Usage :: ./test ip port"
      
      
let _ = 
  let sock = Network_tool.open_connection ip port in
Printf.printf "Connection success !\n" ;
  Unix.close sock
   
