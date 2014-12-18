type action_state = Idle | Waiting | Received of Action.t

class client_player : (Types.update -> unit) -> ?id:Types.id_player -> object


  inherit Player.player

  method event_state : action_state

  method set_state : action_state -> unit

  method get_next_action : Action.t


  method update : Types.update -> unit

end
