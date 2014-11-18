type t = <
  (*TO DO: The type Unit.t list could (should) not be exposed. Instead,
    we could create a module allowing some functions as search one unit,
    get a list of unit etc... *)

  get_id : int;
  get_army : Unit.t list;
  add_unit : Unit.t -> unit;
  set_army : Unit.t list -> unit;
  set_buildings : Building.t list -> unit;
  get_buildings : Building.t list;
  add_building : Building.t -> unit;
  
  (* set_buildings : Building.t list -> unit; *)

  (* not implemented yet *)
  delete_unit : Unit.t -> unit;
  move_unit : Unit.t -> Path.t -> unit;
  delete_building : Building.t -> unit;
  
  get_next_action :  Action.t;    (* Appelé par la grosse boucle, renvoie depuis la file l'action que veut effectuer le joueur *)
>

val create_player : unit -> t

