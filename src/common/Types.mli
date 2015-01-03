(*A first draft *)
(** @author Paul-Gallot Julien Grange et François Thiré *)


type error = Wrong_id_player

type id_player = int

(** Type of the data sent from the engine to the player/client *)
type update =
    Game_over
  | Your_turn
  | Classement
  | Set_army of Unit.t list * id_player
  | Set_building of Building.t list * id_player
  | Add_unit of Unit.t * id_player
  | Add_building of Building.t * id_player
  | Delete_unit of Unit.id *id_player (* Fog or kill -- really? *)
  | Delete_building of Building.id * id_player (* fog or kill *)
  | Move_unit of Unit.id * Action.movement * id_player
  | Set_unit_hp of Unit.id * int * id_player
  | Building_changed of Building.t
  | Set_unit_played of Unit.id * id_player * bool
(* for initialization only *)
  | Set_client_player of id_player
  | Set_logic_player_list of id_player list
  | Map of string



(*what a NetPlayer send to a client player*)
type  send = Get_next_action | Update of update

(*What a client player send to a Net Player *)
type  receive = Next_action of Action.t | Error of error
