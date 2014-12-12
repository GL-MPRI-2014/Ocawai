type action_state = Idle | Waiting | Received of Action.t

class client_player : ?id:Types.id_player -> Unit.t list -> Building.t list -> object

  inherit Player.player

  method event_state : action_state

  method set_state : action_state -> unit

  method get_next_action : Action.t

  method set_logicPlayerList : (Player.logicPlayer list) -> unit

  method get_logicPlayerList : Player.logicPlayer list
end
