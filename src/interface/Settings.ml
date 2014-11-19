type screen_mode = Fullscreen | Windowed

let settings = object(self)

  val mutable mode = Fullscreen
  val mutable cursor_speed = 20.
  val mutable zoom_speed = 10.

  method set_mode v = mode <- v
  method mode = mode

  method set_cursor_speed v = cursor_speed <- v
  method cursor_speed = cursor_speed

  method set_zoom_speed v = zoom_speed <- v
  method zoom_speed = zoom_speed

end
