(** @author Paul-Gallot Julien Grange et François Thiré *)

(** Error that can be sent by the engine if a client use a wrong id*)
type error = Wrong_id_player


type id_player = int

(** Type of the data sent from the engine to the player/client *)
type update =
  | Game_over
  | Your_turn
  | Turn_of of id_player
  | Classement
  | Set_army of Unit.t list * id_player
  | Set_building of Building.t list * id_player
  | Add_unit of Unit.t * id_player
  | Add_building of Building.t * id_player
  | Delete_unit of Unit.unit_id *id_player (* Fog or kill -- really? *)
  | Delete_building of Building.building_id * id_player (* fog or kill *)
  | Move_unit of Unit.unit_id * Action.movement * id_player
  | Set_unit_hp of Unit.unit_id * int * id_player
  | Building_changed of Building.t
  | Set_unit_played of Unit.unit_id * id_player * bool
  | Harvest_income
  | Use_resource of int
(* for initialization only *)
  | Set_client_player of id_player
  | Set_logic_player_list of id_player list
  | Map of string


val get_next_action_code : int
val update_code : int
val next_action_code : int
val error_code : int

val clock : float


(*What a client player send to a Net Player *)
(*type  receive = Next_action of Action.t | Error of error*)


val from_string : string -> update
val to_string : update -> string


