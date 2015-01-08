open ParticleManager
open ParticleMixins

let rec random_color () = 
  let (a,b,c) = (Random.int 2, Random.int 2, Random.int 2) in
  if a = 0 && b = 0 && c = 0 then random_color ()
  else OcsfmlGraphics.Color.rgb (a * 255) (b * 255) (c * 255)


class circle_particle ~maxspeed ~position ~color = 
 
  let rotation = Random.float 6.283 in
  let absspeed = ((min (Random.float 4.) 1.) *. (Random.float 0.35 +. 0.7)) in
  let absspeed = maxspeed *. absspeed in
  
  object(self)
  
  inherit base_particle ~position ~color ~rotation ~scale:1.
    ~life:(Random.float 2. +. 0.5) 
    ~speed:(absspeed *. (cos rotation), absspeed *. (sin rotation))

  inherit slowing_particle 0.985

  inherit prop_speed_scale 0.004

  inherit prop_life_alpha 5.

end

let buf = new OcsfmlAudio.sound_buffer (`File("./resources/sounds/explosion-01.wav"))
let sound = new OcsfmlAudio.sound ~buffer:buf ()

let rec boom_circle (manager : particle_manager) maxspeed position color = function
  |0 -> ()
  |n -> 
    let p = new circle_particle ~maxspeed ~position ~color in
    manager#add_particle p;
    (*sound#play;*)
    Sounds.play_sound "explosion-01";
    boom_circle manager maxspeed position color (n-1)
    

class grav_particle ~maxspeed ~position ~color ~rotation = 
 
  let absspeed = ((min (Random.float 4.) 1.) *. (Random.float 0.15 +. 0.9)) in
  let absspeed = maxspeed *. 2. *. absspeed in
  
  object(self)
  
  inherit base_particle ~position ~color ~rotation ~scale:1.
    ~life:(Random.float 2. +. 0.5) 
    ~speed:(absspeed *. (cos rotation), absspeed *. (sin rotation))

  inherit slowing_particle 0.99

  inherit prop_speed_scale 0.005

  inherit prop_speed_rotation 

  inherit gravity_pull (500.)

  inherit prop_life_alpha 5.

end 

let continuous_fountain (manager : particle_manager) position angle var color = 
  for i = 0 to 5 do 
    let p = new grav_particle 
      ~maxspeed:(Random.float 500. +. 900.)
      ~position
      ~rotation:(angle +. (Random.float var) -. (var /. 2.))
      ~color
    in
    manager#add_particle p
  done

let continuous_circle (manager : particle_manager) position color density = 
  for i = 0 to density do
    let p = new circle_particle
      ~maxspeed:700.
      ~position ~color
    in manager#add_particle p
  done

let continuous_flower (manager : particle_manager) base = 
  let center = Utils.addf2D base (0., -.500.) in
  let new_center theta = 
    let offset = (200. *. cos theta, 200. *. sin theta) in
    Utils.addf2D center offset
  in
  let pi = 3.141592 in
  let petal_centers = [
    new_center (0. *. pi /. 5.);
    new_center (2. *. pi /. 5.);
    new_center (4. *. pi /. 5.);
    new_center (6. *. pi /. 5.);
    new_center (8. *. pi /. 5.)]
  in
  continuous_circle manager center (OcsfmlGraphics.Color.rgb 240 240 20) 15;
  List.iter 
    (fun p -> continuous_circle manager p (OcsfmlGraphics.Color.rgb 255 0 0) 5)
    petal_centers;
  continuous_fountain manager base (-.pi/.2.) 0.10 (OcsfmlGraphics.Color.rgb 0 255 0)

