
(* SEND *)

val send : Unix.file_descr -> int -> string -> float -> bool


(* RECV *)

val recv :  Unix.file_descr -> float -> (int * string) option
