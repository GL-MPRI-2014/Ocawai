(** Module containing the core functions of the script interpreter *)

exception Do_nothing

exception End_turn

(** Exposes the core functions *)
val init : unit -> unit
