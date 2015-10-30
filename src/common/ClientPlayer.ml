open Player

type action_state = Idle | Waiting | Received of Action.t


class client_player ?(id) add_update get_next_action =

  object (self)

  inherit player ?id:id () as super

  val mutable logicPlayerList = []

  method get_next_action = get_next_action ()

  method update (u:Types.update) = add_update u



end
