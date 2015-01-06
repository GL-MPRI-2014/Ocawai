class virtual slowing_particle h = object(self)

  val virtual mutable speed : float * float

  method virtual add_update : (float -> unit) -> unit

  initializer
    self#add_update (fun dt -> 
      let (sx, sy) = speed in 
      let coeff = (1. -. h) ** dt in
      speed <- (coeff *. sx, coeff *. sy))

end

class virtual prop_speed_scale fact = object(self)

  val virtual mutable speed : float * float

  val virtual mutable scale : float

  method virtual add_update : (float -> unit) -> unit

  initializer
    self#add_update (fun dt ->
      let (sx, sy) = speed in 
      let n = sqrt (sx *. sx +. sy *. sy) in
      scale <- min 1. (n *. fact))

end

class virtual prop_speed_rotation = object(self)

  val virtual mutable speed : float * float

  val virtual mutable rotation : float

  method virtual add_update : (float -> unit) -> unit

  initializer
    self#add_update (fun dt ->
      let (sx, sy) = speed in 
      rotation <- 
        if sx > 0. then atan (sy /. sx)
        else if sx < 0. then atan (sy /. sx) +. 3.141592
        else if sy < 0. then -. 3.141592 /. 2.
        else 3.141592 /. 2.)

end

class virtual gravity_pull g = object(self)

  val virtual mutable speed : float * float

  method virtual add_update : (float -> unit) -> unit

  initializer
    self#add_update (fun dt -> 
      let (sx, sy) = speed in
      speed <- (sx, sy +. dt *. g)
    )
end

class virtual prop_life_alpha coeff = object(self)

  val virtual mutable color : OcsfmlGraphics.Color.t

  method virtual life_ratio : float

  method virtual add_update : (float -> unit) -> unit

  initializer
    self#add_update (fun dt ->
      let value = 1. -. self#life_ratio in
      let alpha = int_of_float ((min 1. (coeff *. value)) *. 255.) in
      color <- OcsfmlGraphics.Color.(rgba color.r color.g color.b alpha)
    )
end

