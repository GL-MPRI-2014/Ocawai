open ParticleManager

let rec boom_circle (manager : particle_manager) position color = function
  |0 -> ()
  |n -> 
    let rotation = Random.float 6.283 in
    let absspeed = Random.float 100. in
    let p = new base_particle ~position ~color
    ~rotation ~scale:1. ~life:(Random.float 2. +. 2.)
    ~speed:(absspeed *. (cos rotation), absspeed *. (sin rotation))
    in manager#add_particle p;
    boom_circle manager position color (n-1)
    

  
