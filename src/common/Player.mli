
type t = <
  get_army : Unit.t list;
  set_army : Unit.t list -> unit;

  add_unit : Unit.t -> unit;

  get_buildings : Building.t list;
  set_buildings : Building.t list -> unit;

  add_building : Building.t -> unit;

  get_next_action : Action.t;    (* Appelé par la grosse boucle, renvoie depuis la file l'action que veut effectuer le joueur *)
  push_next_action : Action.t -> unit; (* Appelé par l'interface, permet d'envoyer dans le file l'action que veut effectuer le joueur *)

  last_game_update : Action.t; (* Appelé par l'interface, renvoie la dernière action effectuer sur le jeu par n'importe quelle player. *)
  push_last_game_update : Action.t -> unit (* Appelé par la GB, envoie la dernière action effectuer *)
>

val create_player : unit -> t
