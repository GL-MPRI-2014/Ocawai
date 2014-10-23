class building =
object (self)
  val mutable name = ""
  val mutable product = ([] : Unit.t list)
  (* val mutable income = Resource.create 0 *) (* Resource à implanter *)
  val mutable pos = Position.create (0,0)
  method get_name = name
  method get_producible = product
  (* method get_income = income *)
  method get_position = pos
end

type t = building
