(** Represents a player *)

(**Raise by the function use_resource*)
exception Not_enough_ressource

(**A logic player is an abstract player. It represents the vision for one player of its opponents.
@param id a logic player can be created with a specific id.*)
class logicPlayer : ?id:Types.id_player -> unit ->
object

  (** Get the units visible by the player due to the fog
      @return units visible by the player*)
  method get_visible_army_for : logicPlayer -> Unit.t list

  (**@return The fog for the player*)
  method get_fog : Fog.t


  (**Get the id of the player*)
  method get_id : Types.id_player

  (** Returns true iff there is at least one playable unit *)
  method has_playable_unit : bool

  (** Get the buildings owned by the player*)
  method get_army : Unit.t list

  (** Add a unit to the player*)
  method add_unit : Unit.t -> unit

  (** Set the units to the player. Useful for the initialisation.*)
  method set_army : Unit.t list -> unit

  (** If the player has a base, return its base as a building*)
  method get_base : Building.t option

  (**Set  the base of the player*)
  method set_base : Building.t -> unit

  (** Set the buildings to the player. Useful for the initialisation.*)
  method set_buildings : Building.t list -> unit

  (** Get the buildings owned by the player*)
  method get_buildings : Building.t list

  (** Add a building to the player*)
  method add_building : Building.t -> unit
  (* TODO *)
  method set_unit_hp : Unit.id -> int -> unit


  (**Get a unit owned by the player using the unit id.
   @return the unit corresponding to the id*)
  method get_unit_by_id : Unit.id -> Unit.t

  (**Get a building owned by the player using the building id.
   @return the building corresponding to the id*)
  method get_building_by_id : Building.id -> Building.t

  (**Delete a unit owned by the player using the id of the unit*)
  method delete_unit : Unit.id -> unit
  (**Delete a building owned by the player using the id of the unit*)
  method delete_building : Building.id -> unit
  (**Move the unit referenced by its id. Action.movement is the path of this unit. *)
  method move_unit : Unit.id -> Action.movement -> unit

  (** Get the amount of resource owned by the player*)
  method get_value_resource : int

  (**Check if the player has enough ressource.
   @return True if it is possible, False otherwise*)
  method has_resource : int -> bool
  (**Try to use resource from the player. Raise en exception if the player has not enough resource*)
  method use_resource : int -> unit

  (**Harvest the resources producted by the buildings owned by the player*)
  method harvest_buildings_income : unit

  method init : Battlefield.t -> logicPlayer list -> unit
end
(**A player is a enhanced logic player. It has a get_next_action that ask for the next action of the player. And it knows the other players on the game.
@param id a logic player can be created with a specific id.*)
class virtual player : ?id:Types.id_player -> unit ->
object
  inherit logicPlayer

  (**Get the next action of the player*)
  method virtual get_next_action :  Action.t

  (**Set the logic player (the opponents) of this player.*)
  method set_logic_player_list : (logicPlayer list) -> unit

  (*TODO : maybe this method is useless*)
  (**Get all the logic player of this player : probably useless*)
  method get_logic_player_list : logicPlayer list

  (**A player can receive an update. It can be an action triggered by itself or by its opponents.*)
  method virtual update : Types.update -> unit

  end

(**Create a new player*)
val create_player : unit -> player

(**Create a new dummy player. A dummy player has a predefined set of actions. Useful for tests.*)
val create_dummy_player : Action.t list ->  player
