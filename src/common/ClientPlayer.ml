open Player

type action_state = Idle | Waiting | Received of Action.t

class client_player (a : Unit.t list) (b : Building.t list) = 

  object (self) 

  inherit player a b

  val mutable event_state = Idle

  method event_state = event_state

  method set_state s = event_state <- s

  method get_next_action = 
    event_state <- Waiting;
    let rec get_aux () = 
      match event_state with
      |Received(a) -> a
      | _ -> get_aux ()
    in get_aux ()
    

end
