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
  method percentage_light = pl
  method percentage_normal = pn
  method percentage_heavy = ph
  method price = price
  method life_max = l_m
end

class soldier (s : string) (p_id : int) (p : Position.t) (m : movement)
  (v :int) (min_a : int) (a : int) (r : int) (sp : int) (ab : int) (ar : armor) (pl : int) (pn : int)
  (ph :int) (price : int) (l_m : int)=
object (self)
  inherit unbound_soldier s m v min_a a r sp ab ar pl pn ph price l_m
  val mutable pos = p
  val mutable life = l_m
  val mutable played = false
  val mutable id = 0
  method hp = life
  method get_id = id
  method player_id = p_id
  method position = pos
  method move newpos = pos<-newpos
  method attack arm a = 
  (* fonction affine gérée par l'engine -> gérer les ripostes *)
    let _ = Random.self_init() in
    let r = 85 + Random.int 31 in
    match arm with 
      | Light -> a*self#attack_base*self#percentage_light*r/1000000
      | Normal -> a*self#attack_base*self#percentage_normal*r/1000000
      | Heavy -> a*self#attack_base*self#percentage_heavy*r/1000000
  method take_damage dmg = life <- max 0 (life-dmg)
  method has_played = played
  method set_played p = played <- p

  initializer id <- Oo.id self
end


type id = int
type t = soldier
type unbound_t = unbound_soldier

let bind uu pos p_id =
  new soldier uu#name p_id pos uu#movement_type 
    uu#vision_range uu#min_attack_range uu#attack_range uu#move_range 
    uu#spawn_number uu#attack_base uu#armor uu#percentage_light uu#percentage_normal uu#percentage_heavy uu#price uu#life_max

let create_unbound_from_parsed_unit u = new unbound_soldier (u.Unit_t.name)
(match (u.Unit_t.movement_type) with
| `Walk -> Walk
| `Roll -> Roll
| `Tread -> Tread
| `Swim -> Swim
| `Fly -> Fly
| `Amphibious_Walk -> Amphibious_Walk
| `Amphibious_Roll -> Amphibious_Roll
| `Amphibious_Tread -> Amphibious_Tread
| `All -> All
) (u.Unit_t.vision_range) (u.Unit_t.attack_range_min) (u.Unit_t.attack_range_max) 
(u.Unit_t.move_range) (u.Unit_t.spawn_number) (u.Unit_t.attack_base)
(match u.Unit_t.armor with
| `Light -> Light
| `Normal -> Normal
| `Heavy -> Heavy
) (u.Unit_t.percentage_light) (u.Unit_t.percentage_normal) (u.Unit_t.percentage_heavy)
(u.Unit_t.price) (u.Unit_t.life_max)

open Unit_t 
let create_parsed_unit_from_unbound (u :unbound_t) =
  {
    name = u#name;
    movement_type = (match u#movement_type with
    | Walk -> `Walk
    | Roll -> `Roll
    | Tread -> `Tread
    | Swim -> `Swim
    | Fly -> `Fly
    | Amphibious_Walk -> `Amphibious_Walk
    | Amphibious_Roll -> `Amphibious_Roll
    | Amphibious_Tread -> `Amphibious_Tread
    | All -> `All);
    vision_range = u#vision_range;
    attack_range_min = u#min_attack_range;
    attack_range_max = u#attack_range;
    move_range = u#move_range;
    spawn_number = u#spawn_number;
    attack_base = u#attack_base;
    armor = (match u#armor with
    | Light -> `Light
    | Normal -> `Normal
    | Heavy -> `Heavy);
    percentage_light = u#percentage_light;
    percentage_normal = u#percentage_normal;
    percentage_heavy = u#percentage_heavy;
    price = u#price;
    life_max = u#life_max;
  }

