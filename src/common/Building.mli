(** Building interface (draft) *)

type t = <
  name : string;
  position : Position.t;
  id : int;
  player_id : int option;
  producible : Unit.t list;
  set_owner : int -> unit;
  set_neutral : unit
>
