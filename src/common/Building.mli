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

type id

type t = <
  name : string;
  position : Position.t;
  get_id : id;
  player_id : string;
  producible : Unit.t list
>
