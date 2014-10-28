(* Dummy Unit implementation to test interface *)

type movement = Walk | Roll | Tread | Swim | Fly | Amphibious_Walk | Amphibious_Roll | Amphibious_Tread

class soldier (s : string) (p : Position.t) (m : movement)
  (v :int) (a : int) (r : int) (sp : int) =
object (self)
  val name = s
  val mutable pos = p
  method name = name
  method position = pos
  method move newpos = pos<-newpos
  method movement_type = m
  method vision_range = v
  method attack_range = a
  method move_range = r
  method spawn_number = sp
end

type t = soldier

let create_from_unit_t u pos = new soldier (u.Unit_t.name) pos (match (u.Unit_t.movement_type) with
                                                  | "walk" -> Walk
                                                  | "roll" -> Roll
                                                  | "tread" -> Tread
                                                  | "swim" -> Swim
                                                  | "fly" -> Fly
                                                  | "amphibious_walk" -> Amphibious_Walk
                                                  | "amphibious_roll" -> Amphibious_Roll
                                                  | "amphibious_tread" -> Amphibious_Tread
                                                  | a -> failwith("unit_t_to_t : "^a^" is not a movement\n")
) (u.Unit_t.vision_range) (u.Unit_t.attack_range) (u.Unit_t.move_range) (u.Unit_t.spawn_number)

let create_from_file s1 s2 pos =
  let ui =List.find
    (fun uni -> uni.Unit_t.name = s1)
    (Ag_util.Json.from_file Unit_j.read_t_list s2) in
  create_from_unit_t ui pos

let create_from_config s1 = create_from_file s1 "resources/config/units.json"

