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


let rec boom_circle (manager : particle_manager) maxspeed position color = function
  |0 -> ()
  |n -> 
    let p = new circle_particle ~maxspeed ~position ~color in
    manager#add_particle p;
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

let continuous_fountain (manager : particle_manager) position angle = 
  for i = 0 to 5 do 
    let p = new grav_particle 
      ~maxspeed:(Random.float 500. +. 900.)
      ~position
      ~rotation:(angle +. (Random.float 0.3) -. 0.15)
      ~color:(OcsfmlGraphics.Color.rgb 220 220 20)
    in
    manager#add_particle p
  done


