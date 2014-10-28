class cursor ~position = object(self)

  val mutable current_position : Position.t = position
  val mutable moving = false
  val mutable path = Path.empty
  val mutable scale = 1.

  initializer
    (* Let's interpolate that ! *)
    let my_fun t =
      scale <- 1. +. (sin (t *. 3.)) /. 20.
    in
    ignore (Interpolators.new_ip_from_fun my_fun)

  method set_position pos =
    current_position <- pos ;
    if moving then path <- Path.reach path pos

  method position =
    current_position

  method set_moving =
    moving <- true ;
    path <- Path.init current_position

  method stop_moving =
    moving <- false ;
    path <- Path.empty

  method toggle_moving =
    if moving then self#stop_moving
    else self#set_moving

  method get_move =
    Path.get_move path

  method scale = scale
end
