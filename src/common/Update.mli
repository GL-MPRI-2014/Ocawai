(*A first draft *)

(*what a NetPlayer send to a client player*)
type  send = Get_next_action | Update of update

(*What a client player send to a Net Player *)
type  receive = Next_action of Action.t

type Id_player = int
  
type update =
    Game_over
  | Classement 
  | Set_unit of Unit.t list * Id_player
  | Set_building of Building.t list * Id_player
  | Add_unit of Unit.t * Id_player
  | Add_building of Building.t * Id_player
  | Delete_unit of Unit.t *Id_player (*Fog or kill*)
  | Delete_building of Building.t * Id_player(*fog or kill*)
  | Move_unit of Unit.t * Path.t list * Id_player
                       
                         
