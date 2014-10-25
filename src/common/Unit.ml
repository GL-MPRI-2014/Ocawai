(* Dummy Unit implementation to test interface *)

type movement = Walk | Roll | Tracks | Swim | Fly | Amphibious_Walk | Amphibious_Roll | Amphibious_Tracks

class soldier (s : string) (p : Position.t) (m : movement) 
  (v :int) (a : int) (r : int) = 
object (self)
  val name = s
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
let create_from_file s1 s2 pos = 
  new soldier s1 pos Walk 3 2 4
