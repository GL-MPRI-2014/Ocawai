class cursor ~position = object(self)

  val mutable current_position : Position.t = position

  method set_position pos =
    current_position <- pos

  method position = current_position

end
