
(** Represent the current state of a client_player.
    Idle -> Waiting to play
    Waiting -> wait an action from the player
    Received -> A new action from the player that the engine need to handle*)
type action_state = Idle | Waiting | Received of Action.t

(** A player that receives updates from the engine. A client player take a function that handle
    updates from the engine. *)
class client_player : ?id:Types.id_player -> (Types.update -> unit) -> object

  inherit Player.player

  (** Get the event status of the player*)
  method event_state : action_state

  (** Set the current status of the player*)
  method set_state : action_state -> unit

  (**@See Player.mli*)
  method get_next_action : Action.t

  (** Handle a new update from the engine *)
  method update : Types.update -> unit

end
