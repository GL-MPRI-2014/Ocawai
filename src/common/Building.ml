class building (p_id : string) (p : Position.t) =
object (self)
  val name = ""
  val mutable product = ([] : Unit.t list)
  (* val mutable income = Resource.create 0 *) (* Resource à implanter *)
  val mutable pos = p
  val mutable id = 0
  method name = name
  method get_id = id
  method player_id = p_id
  method producible = product
  (* method get_income = income *)
  method position = pos

  initializer id <- Oo.id self
end

type t = building
type id = int
