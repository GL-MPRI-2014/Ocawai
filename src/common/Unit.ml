(* Dummy Unit implementation to test interface *)

type t = {name : string; pos : Position.t}

type movement = Walk | Swim | Fly | Amphibious

let get_name t = t.name

let get_position t = t.pos

let movement_type t = Walk

let vision_range t = 3

let attack_range t = 2

let move_range t = 4

(* Wow. Much bad. *)
let create_from_file s1 s2 = 
  {name = "infantry"; 
   pos = Position.create (int_of_string s1, int_of_string s2)}
