class unbound_building (s : string) (prod : Unit.t list) (inc : int) =
object (self)
  val name = s
  val product = prod
  val income = inc
  method name = name
  method producible = product
  method get_income = income
end


class building (ub : unbound_building) (p : Position.t) (p_id : int option) 
  (id0 : int) =
object (self)
  inherit unbound_building ub#s ub#prod ub#inc
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
  new unbound_building (pb.Building_t.name) (pb.Building_t.product) 
    (pb.Building_t.get_income)

let create_parsed_unit_from_unbound ub =
  let open Building_t in
  {
    name = ub#name;
    product = ub#product;
    get_income = ub#get_income
  }


