(**
 * This module is designed to demarshalization the program.
 * It implement the functions of sending and receiving data.
 *)


(* SEND *)

(**
 * This is function to translate an integer to string.
 * @param n Integer to be translate
 * @param size Length of result string
 *)
let int2string n length  =
  let result = String.create length in
  for j = (length) downto 1 do
      result.[length - j] <- (char_of_int ((n lsr (8 * (j - 1))) mod 256))
  done;
  result


(**
 * This function is the opposite translation.
 *)
let rec string2int string = function
    | 0 -> 0
    | n -> (int_of_char string.[n - 1]) + (256 * (string2int string (n-1)))


(**
 * This function send bytes regardless of protocol.
 * @param sock File descriptor
 * @param string Bytes to send
 * @param break_time Limit time of the operation
 * @return [true] if successful in shipment and [false] otherwise
 *)
let rec send_string sock string break_time =
  let timeout = max (break_time -. Sys.time ()) 0. in
  let length = String.length string in

  let size = Network_tool.write_timeout sock string length timeout in

  match size with
    | None -> false
    | Some(n) when length = n -> true
    | Some(n) ->
      begin
	let rest = String.sub string n (length - n) in
	send_string sock rest break_time
      end	


(**
 * This function send bytes with the protocol.
 * @param sock File descriptor
 * @param magic Code of function to send
 * @param string Bytes to send
 * @param break_time Limit time of the operation
 * @return [true] if successful in shipment and [false] otherwise
 *)
let send sock magic string timeout =
  let break_time = timeout +. Sys.time () in
  let magic_char = char_of_int magic in
  let magic_string = String.make 1 magic_char in
  let length_string =  int2string (String.length string) 4 in
  let data = String.concat "" [magic_string;length_string;string] in

  Log.infof "SEND\n";
  Log.infof "\tMAGIC : %d (with intention %d)\n" (int_of_char data.[0]) magic;
  Log.infof "\tLENGTH : %d (with intention %d)\n" (string2int (String.sub data 1 4) 4)  (String.length string);

  send_string sock data break_time



(* RECV *)

(**
 * This function receive bytes regardless of protocol.
 * @param sock File descriptor
 * @param length Number of byte to receive
 * @param break_time Limit time of the operation
 * @return [true] if successful on receipt and [false] otherwise
 *)
let recv_string sock length break_time =

  let rec recv_string' sock accum length break_time =
    let timeout = break_time -. Sys.time () in
    let buffer = String.create length in
    
    try
      let size = Network_tool.read_timeout sock buffer length timeout in

      match size with
	| None -> None
	| Some(0) -> None
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
	    
    with
      | Unix.Unix_error (_, _, _) ->
        Printf.printf "Client disconnected !\n";
	None
  in

  if length <= 0 then 
    Some("")
  else
    recv_string' sock [] length break_time


(**
 * This is an equivalent of reverse application operator [|>] but for options.
 * @param arg THE argument option of the function
 * @param funct Application that expects the argument
 * @return [None] if [arg = None] and [funct(arg)] otherwise
 **)
let (|>>) arg funct =
  match arg with
    | None -> None
    | Some value -> funct value


(**
 * This function translates and combines the results of the fragmented receipt
 * @param sock File descriptor
 * @param break_time Limit time of the operation
 * @param magic_string Result of [recv_length]
 * @param data Result of [recv_data]
 * @return [None] if successful on receipt and [None] otherwise
 *)
let combine_results sock break_time magic_string data =
  let magic = int_of_char magic_string.[0] in
  Some (magic, data)


(**
 * This function receive data of receipt
 * @param sock File descriptor
 * @param break_time Limit time of the operation
 * @param magic_string Result of [recv_length]
 * @return [None] if successful on receipt and [None] otherwise
 *)
let recv_data sock break_time magic_string length_string =
  let length = string2int length_string 4 in

  Log.infof "RECV\n";
  Log.infof "\tMAGIC : %d\n" (int_of_char magic_string.[0]);
  Log.infof "\tLENGTH : %d\n" length;

  let data = recv_string sock length break_time in
  data |>> (combine_results sock break_time magic_string)


(**
 * This function receive the number of byte of receipt
 * @param sock File descriptor
 * @param break_time Limit time of the operation
 * @param magic_string Result of [recv_magic]
 * @return [None] if successful on receipt and [None] otherwise
 *)
let recv_length sock break_time magic_string =
  let length_string = recv_string sock 4 break_time in
  length_string |>> (recv_data sock break_time magic_string)


(**
 * This function receive the code of function of receipt
 * @param sock File descriptor
 * @param break_time Limit time of the operation
 * @return [None] if successful on receipt and [None] otherwise
 *)
let recv_magic sock break_time =
  let magic_string = recv_string sock 1 break_time in
  magic_string |>> (recv_length sock break_time)


(**
 * This function receive bytes with the protocol.
 * @param sock File descriptor
 * @param timeout Waiting time
 * @return [Some(magic, string)] if successful on receipt and [None] otherwise
 *)
let recv sock timeout =
  let break_time = Sys.time () +. timeout in
  recv_magic sock break_time









    



