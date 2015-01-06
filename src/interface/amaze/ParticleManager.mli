class base_particle : position:(float*float) -> rotation:float -> 
  speed:(float*float) -> scale:float -> color:OcsfmlGraphics.Color.t ->
  life:float -> object

  val mutable position : float * float
  val mutable rotation : float
  val mutable speed    : float * float
  val mutable scale    : float
  val mutable color    : OcsfmlGraphics.Color.t
  
  method life_ratio : float
  
  method update : float -> unit

  method add_update : (float -> unit) -> unit

  method is_alive : bool

  method render : OcsfmlGraphics.texture -> OcsfmlGraphics.render_texture ->
    unit

end


class particle_manager : OcsfmlGraphics.render_window -> object

  method add_particle : #base_particle -> unit

  method update : unit

  method render : unit

end
