(** Action functions *)

type movement = Position.t list

type action = 
| Attack_unit of (Unit.t * Unit.t)
| Attack_building of (Unit.t * Building.t)
| Wait
| End_turn

type t = movement * action
  
exception Bad_unit
exception Has_played
exception Bad_path
exception Bad_attack

