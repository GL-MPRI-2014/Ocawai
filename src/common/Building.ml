class building (p : Position.t) =
object (self)
  val name = ""
  val mutable product = ([] : Unit.t list)
  (* val mutable income = Resource.create 0 *) (* Resource à implanter *)
  val mutable pos = p
  val mutable player_id = None
  method name = name
  method id = Oo.id self
  method player_id = player_id
  method producible = product
  (* method get_income = income *)
  method position = pos
  method set_owner (p_id : int) = player_id <- Some p_id
  method set_neutral = player_id <- None
end

type t = building
type id = int
