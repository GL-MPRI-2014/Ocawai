module type FAMILY =
sig
  type 'a family
  val add_element :  'a family -> 'a  -> 'a family
  val remove_element : 'a family -> 'a -> 'a family
  val new_family : () -> 'a family
  exception family_is_empty
end

module 'a List_family : FAMILY =
struct
  type 'a family = 'a list
  let add_element list elem = elem::list
                              
  let new_family () = []
end

type t = <
  (*TO DO: The type Unit.t list could (should) not be exposed. Instead,
    we could create a module allowing some functions as search one unit,
    get a list of unit etc... *)
  get_army : Unit.t list;
  set_army : Unit.t list -> unit;

  add_unit : Unit.t -> unit;
  (*remove_unit : Unit.t -> unit;*)
  
  get_buildings : Building.t list;
  set_buildings : Building.t list -> unit;

  add_building : Building.t -> unit;
  (*remove_building : Building.t -> unit;*)
  
  get_next_action : Action.t;    (* Appelé par la grosse boucle, renvoie depuis la file l'action que veut effectuer le joueur *)
>

val create_player : unit -> t
