(* Dummy Unit implementation to test interface *)

type movement = Walk | Roll | Tread | Swim | Fly | Amphibious_Walk
  | Amphibious_Roll | Amphibious_Tread | All

class unbound_soldier (s : string) (m : movement) (v : int) (min_a : int)
  (a : int) (r : int) (sp : int) (ab : int) (ar : armor) (pl : int) (pn : int)
  (ph :int) (price : int)=
object (self)
  val name = s
  method name = name
  method movement_type = m
  method vision_range = v
  method min_attack_range = min_a
  method attack_range = a
  method move_range = r
  method spawn_number = sp
  method attack_base = int
  method armor = ar
  method price = price
  method percentage_light = pl
  method percentage_normal = pn
  method percentage_heavy = ph
end

class soldier (s : string) (p_id : string) (p : Position.t) (m : movement)
  (v :int) (min_a : int) (a : int) (r : int) (sp : int) =
object (self)
  inherit unbound_soldier s m v min_a a r sp
  val mutable pos = p
  method id = string_of_int (Oo.id self)
  method player_id = p_id
  method position = pos
  method move newpos = pos<-newpos
  method attack u a b = match u#armor with 
  (* fonction affine gérée par l'engine -> gérer les ripostes *)
    | Light -> u#hp <- u#hp - a*attack_base*percentage_light/100 + b
    | Normal -> u#hp <- u#hp - a*attack_base*percentage_normal/100 + b
    | Heavy -> u#hp <- u#hp - a*attack_base*percentage_heavy/100 + b
end

type t = soldier
type unbound_t = unbound_soldier

let bind uu pos p_id =
  new soldier uu#name p_id pos uu#movement_type 
    uu#vision_range uu#min_attack_range uu#attack_range uu#move_range 
    uu#spawn_number

let create_unbound_from_unit_t u = new unbound_soldier (u.Unit_t.name) (match (u.Unit_t.movement_type) with
                                                  | "walk" -> Walk
                                                  | "roll" -> Roll
                                                  | "tread" -> Tread
                                                  | "swim" -> Swim
                                                  | "fly" -> Fly
                                                  | "amphibious_walk" -> Amphibious_Walk
                                                  | "amphibious_roll" -> Amphibious_Roll
                                                  | "amphibious_tread" -> Amphibious_Tread
                                                  | "all" -> All
                                                  | a -> failwith("create_unbound_from_unit_t : "^a^" is not a movement\n")
) (u.Unit_t.vision_range) (u.Unit_t.attack_range_min) (u.Unit_t.attack_range_max) (u.Unit_t.move_range) (u.Unit_t.spawn_number)

let create_list_from_file s1 =
  List.map create_unbound_from_unit_t (Ag_util.Json.from_file Unit_j.read_t_list s1)

let create_from_file s1 s2 =
  List.find
    (fun uni -> uni#name = s1)
    (create_list_from_file s2)

let create_list_from_config () = create_list_from_file "resources/config/units.json"

let create_from_config s1 = create_from_file s1 "resources/config/units.json"

