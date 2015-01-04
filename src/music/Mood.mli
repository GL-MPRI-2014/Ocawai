(**
 This gives an idea of the current mood of the player.

 Informations are gathered from the instance of client_data we're given by
 the interface (that is, what the player actually sees).

 @authors Mathias Sablé Meyer
 @authors Théis Bazin
*)

type t = int

exception NotYetInitialised

(**
  Gives you the current mood, a float between -1 and 1, -1 beeing the worst you
  ever saw in this game and 1 beeing the best.
  @returns a float in [-1,1]
*)
val get : unit -> float

(**
  This initiate this module, and requires a client_data that corresponds to the
  player we're playing.
*)
val init : ClientData.client_data -> unit
