(** Building interface (draft) *)

(*
class building :
object
  method get_name : string
  method get_id : string
  method get_player_id : string
  method get_producible : Unit.t list
  (* method get_income : Resource.t *) (* Resource à implanter *)
  method get_position : Position.t
end

type t = building
*)

type t = <
  name : string;
  position : Position.t;
  id : string;
  player_id : string;
  producible : Unit.t list
>
