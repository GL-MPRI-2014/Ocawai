class cursor ~position = object(self)

  val mutable current_position : Position.t = position
  val mutable moving = false
  val mutable path = Pathfinder.init position

  method set_position pos =
    current_position <- pos ;
    path <- Pathfinder.reach path pos

  method position =
    current_position

  method set_moving () =
    moving <- true ;
    path <- Pathfinder.init current_position

  method get_move =
    Pathfinder.get_move path

end
