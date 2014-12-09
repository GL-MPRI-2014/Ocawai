
(* SEND *)


let rec send_string sock string break_time =
  let timeout = break_time -. Sys.time () in
  let length = String.length string in
  let size = Network_tool.write_timeout sock string length 0 timeout in
  
  match size with
    | None -> false
    | Some(n) when length = n -> true
    | Some(n) ->
      begin
	let rest = String.sub string n (length - n) in
	send_string sock rest break_time
      end	



let send sock magic string timeout =
  let break_time = timeout +. Sys.time () in
  let magic_string = string_of_int magic in
  let length_string = string_of_int (String.length string) in 
  let data = String.concat "" [magic_string;length_string;string] in
  send_string sock data break_time



(* RECV *)


let recv_string sock length break_time =

  let rec recv_string' sock accum length break_time =
    let timeout = break_time -. Sys.time () in
    let buffer = String.make length ' ' in
    let size = Network_tool.read_timeout sock buffer length 0 timeout in
    
    match size with
      | None -> None
      | Some(n) when length = n -> 
	begin
	  let string = String.sub buffer 0 n in
	  let rev = List.rev (string::accum) in
	  let data = String.concat "" rev in
	  Some(data)
	end 
      | Some(n) ->
	begin
	  let string = String.sub buffer 0 n in
	  recv_string' sock (string::accum) (length - n) break_time
	end
	  
  in

  if length <= 0 then 
    Some("")
  else
    recv_string' sock [] length break_time



let (|>>) arg funct =
  match arg with
    | None -> None
    | Some value -> funct value



let combine_results sock break_time magic_string data =
  let magic = int_of_string magic_string in
  Some (magic, data)



let recv_data sock break_time magic_string length_string =
  let length = int_of_string length_string in
  let data = recv_string sock length break_time in
  data |>> (combine_results sock break_time magic_string)



let recv_length sock break_time magic_string =
  let length_string = recv_string sock 4 break_time in
  length_string |>> (recv_data sock break_time magic_string)



let recv_magic sock break_time =
  let magic_string = recv_string sock 1 break_time in
  magic_string |>> (recv_length sock break_time)



let recv sock timeout =
  let break_time = Sys.time () +. timeout in
  recv_magic sock break_time







    



