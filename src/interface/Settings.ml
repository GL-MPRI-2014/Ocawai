type screen_mode = Fullscreen | Windowed

let settings = object(self)

  val mutable mode = Fullscreen

  let set_mode smode = mode <- smode

  let mode = mode

end
