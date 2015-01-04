(**
 This gives an idea of the current mood of the player. It will NOT seek for
 informations, it excpects them from elsewhere.
*)

type t = int

exception NotYetInitialised

val init : ClientData.client_data -> unit
val get : unit -> float
