class cursor ~position = object(self)

  val mutable current_position : Position.t = position
  val mutable moving = false
  val mutable path = Pathfinder.init position
  val mutable scale = 1.

  initializer 
    (* Let's interpolate that ! *)
    let my_fun t = 
      scale <- 1. +. (sin (t *. 2.)) /. 20.
    in
    ignore (Interpolators.new_ip_from_fun my_fun)

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

  method scale = scale
end
