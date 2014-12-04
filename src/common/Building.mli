(** Building interface (draft) *)


type id = int
type t = <
  name : string;
  position : Position.t;
  get_id : id;
  player_id : int option;
  producible : Unit.t list;
  set_owner : int -> unit;
  set_neutral : unit
>
