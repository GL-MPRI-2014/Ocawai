class logicPlayer : Unit.t list -> Building.t list  -> object
    method get_id : int
    method get_army : Unit.t list
    method add_unit : Unit.t -> unit
    method set_army : Unit.t list -> unit
    method set_buildings : Building.t list -> unit
    method get_buildings : Building.t list
    method add_building : Building.t -> unit

    method delete_unit : Unit.t -> unit
    method move_unit : Unit.t -> Action.movement -> unit
    method delete_building : Building.t -> unit

  end

class virtual player :  Unit.t list -> Building.t list  -> object
    inherit logicPlayer
    method virtual get_next_action :  Action.t
  end
(*
type logic = <
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

  delete_unit : Unit.t -> unit;
  move_unit : Unit.t -> Action.movement -> unit;
  delete_building : Building.t -> unit;
>
*)

(*
type t = <

  get_id : int;
  get_army : Unit.t list;
  add_unit : Unit.t -> unit;
  set_army : Unit.t list -> unit;
  set_buildings : Building.t list -> unit;
  get_buildings : Building.t list;
  add_building : Building.t -> unit;

  delete_unit : Unit.t -> unit;
  move_unit : Unit.t -> Action.movement -> unit;
  delete_building : Building.t -> unit;
  
  get_next_action :  Action.t;    (* Appelé par la grosse boucle, renvoie depuis la file l'action que veut effectuer le joueur *)
>
*)
val create_player : unit -> player

