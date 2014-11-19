open Unix

val create_listener : int -> file_descr
     
val open_n_connections : int -> int -> file_descr list

val open_connections_timeout : int -> float -> file_descr list

val read_timeout : file_descr -> string -> int -> int -> float -> int option

val  write_timeout : file_descr -> string -> int -> int -> float -> int option
