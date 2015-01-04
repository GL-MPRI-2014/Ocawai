(** @author Paul-Gallot Julien Grange et François Thiré *)


type error = Wrong_id_player

type id_player = int

(** Type of the data sent from the engine to the player/client *)
type update =
    Game_over
  | Classement 
  | Set_army of Unit.t list * id_player
  | Set_building of Building.t list * id_player
  | Add_unit of Unit.t * id_player
  | Add_building of Building.t * id_player
  | Delete_unit of Unit.id *id_player (*Fog or kill*)
  | Delete_building of Building.id * id_player(*fog or kill*)
  | Move_unit of Unit.id * Action.movement * id_player
  | Set_unit_hp of Unit.id * int * id_player
(* for initialization only *)
  | Set_client_player of id_player
  | Set_logic_player_list of id_player list
  | Map of string


let get_next_action_code = 0
let update_code = 1
let next_action_code = 2
let error_code = 3

let clock = 3.0


let from_string (str : string) = 
  Game_over

let to_string (update : update) = 
  ""
