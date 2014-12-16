(** Action functions *)

type movement = Position.t list

type action = 
| Attack_unit of (Unit.t * Unit.t)
| Attack_building of (Unit.t * Building.t)
| Wait
| End_turn

type t = movement * action


 (* TODO: implement this method *)
let from_string (str : string) =
  [Position.create(0,0)],Wait

 (* TODO: implement this method *)
let to_string (a : t) =
  ""
  
exception Bad_unit
exception Has_played
exception Bad_path
exception Bad_attack

