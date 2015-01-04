(* SERVER *)
     
val open_n_connections : int -> int -> Unix.file_descr list

val open_connections_timeout : int -> float -> Unix.file_descr list


(* CLIENT *)

val open_connection : string -> int -> Unix.file_descr


(* COMMUNICATION *)

val read_timeout : Unix.file_descr -> string -> int -> float -> int option

val  write_timeout : Unix.file_descr -> string -> int -> float -> int option
