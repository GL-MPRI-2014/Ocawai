(* Building interface (draft) *)

class building :
object
  method get_name : string
  method get_producible : Unit.t list
  (* method get_income : Resource.t *) (* Resource à implanter *)
  method get_position : Position.t
end

type t = building
