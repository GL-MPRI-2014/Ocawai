type action_state = Idle | Waiting | Received of Action.t

class client_player : ?id:Types.id_player ->
                      (Types.update -> unit) ->
                      (unit -> Action.t) ->
object

  inherit Player.player

  method get_next_action : Action.t

  method update : Types.update -> unit

end
