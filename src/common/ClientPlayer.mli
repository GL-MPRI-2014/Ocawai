
(** Represent the current state of a client_player.
    Idle -> Waiting to play
    Waiting -> wait an action from the player
    Received -> A new action from the player that the engine need to handle*)
type action_state = Idle | Waiting | Received of Action.t


(** A player that receives updates from the engine. A client player take a function that handle
    updates from the engine. *)
class client_player : ?id:Types.id_player ->
                      (Types.update -> unit) ->
                      (Mutex.t -> Action.t) ->
object

  inherit Player.player

  method get_next_action : Mutex.t -> Action.t

  (** Handle a new update from the engine *)
  method update : Types.update -> unit

end
