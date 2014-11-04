class cursor ~position = object(self)

  val mutable current_position : Position.t = position
  val mutable moving = false
  val mutable path = Path.empty
  val mutable scale = 1.

  initializer
    (* Let's interpolate that ! *)
    ignore (Interpolators.new_sine_ip 
      (fun s -> scale <- s) 3. (1./.20.) 1.)

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
