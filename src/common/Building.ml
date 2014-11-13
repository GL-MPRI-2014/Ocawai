class building (p_id : string) (p : Position.t) =
object (self)
  val name = ""
  val mutable product = ([] : Unit.t list)
  (* val mutable income = Resource.create 0 *) (* Resource à implanter *)
  val mutable pos = p
  method name = name
  method id = string_of_int (Oo.id self)
  method player_id = p_id
  method producible = product
  (* method get_income = income *)
  method position = pos
end

type t = building
