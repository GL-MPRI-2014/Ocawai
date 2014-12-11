class unbound_building (s : string) (prod : string list) (inc : int) (sp1 : int) (sp2 : int) (m : Unit.movement list)=
object (self)
  method name = s
  method product = prod
  method income = inc
  method spawn_number_per_player = sp1
  method spawn_number_neutral = sp2
  method movement_types = m
end


class building (ub : unbound_building) (p : Position.t) (p_id : int option) 
  (id0 : int) =
object (self)
  inherit unbound_building ub#name ub#product ub#income ub#spawn_number_per_player ub#spawn_number_neutral ub#movement_types
  val position = p
  val mutable player_id = p_id
  val mutable id = id0
  method position = position
  method get_id = id
  method player_id = player_id
  method set_owner (p_id : int) = player_id <- Some p_id
  method set_neutral = player_id <- None

  initializer if id0 = -1 then id <- Oo.id self
end


type t = building
type unbound_t = unbound_building
type id = int


let bind ub pos p_id =
  new building ub pos p_id (-1)

let bind_extended ub pos p_id id =
  new building ub pos p_id id


let create_unbound_from_parsed_building pb =
  new unbound_building (pb.Building_t.name) pb.Building_t.product
    (pb.Building_t.income) pb.Building_t.spawn_number_per_player pb.Building_t.spawn_number_neutral (let open Unit in List.map (fun mov -> match mov with
| `Walk -> Walk
| `Roll -> Roll
| `Tread -> Tread
| `Swim -> Swim
| `Fly -> Fly
| `Amphibious_Walk -> Amphibious_Walk
| `Amphibious_Roll -> Amphibious_Roll
| `Amphibious_Tread -> Amphibious_Tread
| `All -> All
) pb.Building_t.movement_types)

let create_parsed_building_from_unbound ub =
  let open Building_t in
  {
    name = ub#name;
    product = ub#product;
    income = ub#income;
    spawn_number_per_player = ub#spawn_number_per_player;
    spawn_number_neutral = ub#spawn_number_neutral;
    movement_types = (let open Unit in List.map (fun mov -> match mov with
    | Walk -> `Walk
    | Roll -> `Roll
    | Tread -> `Tread
    | Swim -> `Swim
    | Fly -> `Fly
    | Amphibious_Walk -> `Amphibious_Walk
    | Amphibious_Roll -> `Amphibious_Roll
    | Amphibious_Tread -> `Amphibious_Tread
    | All -> `All)) ub#movement_types;
  }


