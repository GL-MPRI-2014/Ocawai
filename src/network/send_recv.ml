(**
 * This module is designed to demarshalization the program.
 * It implement the functions of sending and receiving data.
 *)


(* SEND *)

let string_of_length length =
  let length_string = string_of_int length in
  let size = String.length length_string in
  

  (* written by Mazzocchi *)
  match size with
  | 0 -> String.make 4 '0' ^ length_string
  | 1 -> String.make 3 '0' ^ length_string
  | 2 -> String.make 2 '0' ^ length_string
  | 3 -> String.make 1 '0' ^ length_string
  | 4 -> length_string
  | _ -> assert false
    

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

  let size = Network_tool.write_timeout sock string 0 length timeout in

  Log.infof "break_time %f systime %f" break_time (Sys.time ());

  Log.infof "In send";
  match size with
    | None -> Log.infof "None";false
    | Some(n) when length = n -> Log.infof "Ok";true
    | Some(n) ->
      begin
	Log.infof "Partiel %d sur %d" n length;
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
  let length_string = string_of_length (String.length string) in
Log.infof "%s" length_string;
  let data = String.concat "" [magic_string;length_string;string] in
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
    let buffer = String.make length ' ' in
    let size = Network_tool.read_timeout sock buffer 0 length timeout in
    
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
  let magic = int_of_string magic_string in
  Some (magic, data)


(**
 * This function receive data of receipt
 * @param sock File descriptor
 * @param break_time Limit time of the operation
 * @param magic_string Result of [recv_length]
 * @return [None] if successful on receipt and [None] otherwise
 *)
let recv_data sock break_time magic_string length_string =
Log.infof "%s" length_string;
  let length = int_of_string length_string in
Log.infof "%s" length_string;
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







    



