class cursor ~position = object

  val mutable current_position = position

  method set_position pos =
    current_position <- pos

  method position = current_position

end
