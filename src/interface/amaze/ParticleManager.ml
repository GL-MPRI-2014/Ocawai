open OcsfmlGraphics

class base_particle ~position ~rotation ~speed 
  ~scale ~color ~life = object(self)

  val mutable position = position
  val mutable rotation = rotation
  val mutable speed    = speed
  val mutable scale    = scale
  val mutable color    = color
  val mutable updates  = []

  val mutable dead = false

  val mutable age = 0.

  initializer
    self#add_update (fun dt -> 
      age <- age +. dt;
      if age > life then self#kill
    );
    self#add_update (fun dt ->
      let spe = (dt *. (fst speed), dt *. (snd speed)) in
      position <- Utils.addf2D position spe)

  method private origin = (scale *. 11., 1.)

  method update dt = List.iter (fun f -> f dt) updates

  method life_ratio = age /. life

  method add_update f = updates <- f :: updates

  method private kill = dead <- true

  method is_alive = not dead

  method render (rect_tex : texture) (target : render_texture) = 
    new sprite ~position ~scale:(scale,1.) 
      ~rotation:(rotation *. 180. /. 3.141592)
      ~texture:rect_tex
      ~color ~origin:self#origin ()
    |> target#draw ~blend_mode:BlendAdd
end


class particle_manager window = 

  let (sx,sy) = window#get_size in

  object(self)

  val mutable particles : base_particle list = []

  val mutable last_update : float = Unix.gettimeofday ()

  val bloomer = BloomEffect.create (sx,sy)

  val rectangle = new texture (`File "resources/textures/gui/rectangle.png")

  val pp_texture = new render_texture sx sy

  method add_particle : 'a. (#base_particle as 'a) -> unit = 
    fun p -> particles <- (p :> base_particle) :: particles

  method update = 
    let dt = Unix.gettimeofday () -. last_update in
    last_update <- last_update +. dt;
    particles <- List.filter (fun p -> p#update dt; p#is_alive) particles

  method render = 
    pp_texture#clear ();
    List.iter (fun p -> p#render rectangle pp_texture) particles;
    pp_texture#display;
    BloomEffect.blooming bloomer pp_texture window
end


    
