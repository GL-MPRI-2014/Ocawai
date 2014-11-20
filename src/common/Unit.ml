(* Dummy Unit implementation to test interface *)

type movement = Walk | Roll | Tread | Swim | Fly | Amphibious_Walk
  | Amphibious_Roll | Amphibious_Tread | All

type armor = Light | Normal | Heavy

class unbound_soldier (s : string) (m : movement) (v : int) (min_a : int)
  (a : int) (r : int) (sp : int) (ab : int) (ar : armor) (pl : int) (pn : int)
  (ph :int) (price : int) (l_m : int)=
object (self)
  method name = s
  method movement_type = m
  method vision_range = v
  method min_attack_range = min_a
  method attack_range = a
  method move_range = r
  method spawn_number = sp
  method attack_base = ab
  method armor = ar
  method price = price
  method percentage_light = pl
  method percentage_normal = pn
  method percentage_heavy = ph
  method life_max = l_m
end

class soldier (s : string) (p_id : int) (p : Position.t) (m : movement)
  (v :int) (min_a : int) (a : int) (r : int) (sp : int) (ab : int) (ar : armor) (pl : int) (pn : int)
  (ph :int) (price : int) (l_m : int)=
object (self)
  inherit unbound_soldier s m v min_a a r sp ab ar pl pn ph price l_m
  val mutable pos = p
  val mutable life = l_m
  method hp = life
  method id = Oo.id self
  method player_id = p_id
  method position = pos
  method move newpos = pos<-newpos
  method attack arm a = 
  (* fonction affine gérée par l'engine -> gérer les ripostes *)
    let _ = Random.self_init() in
    let r = 85 + Random.int 31 in
    match arm with 
      | Light -> a*self#attack_base*self#percentage_light*r/10000
      | Normal -> a*self#attack_base*self#percentage_normal*r/10000
      | Heavy -> a*self#attack_base*self#percentage_heavy*r/10000
  method take_damage dmg = life <- life - dmg
end

type t = soldier
type unbound_t = unbound_soldier

let bind uu pos p_id =
  new soldier uu#name p_id pos uu#movement_type 
    uu#vision_range uu#min_attack_range uu#attack_range uu#move_range 
    uu#spawn_number uu#attack_base uu#armor uu#percentage_light uu#percentage_normal uu#percentage_heavy uu#price uu#life_max

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
) (u.Unit_t.vision_range) (u.Unit_t.attack_range_min) (u.Unit_t.attack_range_max) (u.Unit_t.move_range) (u.Unit_t.spawn_number) (u.Unit_t.attack_base) (match u.Unit_t.armor with | "light" -> Light | "normal" -> Normal | "heavy" -> Heavy | a -> failwith("create_unbound_from_unit_t : "^a^" is not an armor type\n")) (u.Unit_t.percentage_light) (u.Unit_t.percentage_normal) (u.Unit_t.percentage_heavy) (u.Unit_t.price) (u.Unit_t.life_max)

let create_list_from_file s1 =
  List.map create_unbound_from_unit_t (Ag_util.Json.from_file Unit_j.read_t_list s1)

let create_from_file s1 s2 =
  List.find
    (fun uni -> uni#name = s1)
    (create_list_from_file s2)

let create_list_from_config () = create_list_from_file "resources/config/units.json"

let create_from_config s1 = create_from_file s1 "resources/config/units.json"

