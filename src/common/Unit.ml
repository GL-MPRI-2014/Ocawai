(* Dummy Unit implementation to test interface *)

type movement = Walking | Swimming | Flying | Amphibious

class soldier (s : string) (p : Position.t) (m : movement) 
  (v :int) (a : int) (r : int) = 
object (self)
  val mutable name = s
  val mutable pos = p
  method name = name
  method position = pos
  method movement_type = m
  method vision_range = v
  method attack_range = a
  method move_range = r
end

type t = soldier

(* Wow. Much bad. *)
let create_from_file s1 s2 = 
  new soldier "infantry" (Position.create (int_of_string s1, int_of_string s2))
    Walking 3 2 4
