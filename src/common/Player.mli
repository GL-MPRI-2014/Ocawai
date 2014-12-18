(**Represent an item in the log file of the player.*)
type log_item =
  | Moved of Unit.t * Action.movement

(**A logic player is an abstract player. It represents the vision for one player of its opponents.
@param id a logic player can be created with a specific id.*)
class logicPlayer : ?id:Types.id_player -> Unit.t list -> Building.t list  ->
object

  (**Get the id of the player*)
  method get_id : Types.id_player
		    
  (** Get the buildings owned by the player*)
  method get_army : Unit.t list
			   
  (** Add a unit to the player*)
  method add_unit : Unit.t -> unit
				
  (** Set the units to the player. Useful for the initialisation.*)
  method set_army : Unit.t list -> unit

  method get_base : Building.t option
  method set_base : Building.t -> unit

  (** Set the buildings to the player. Useful for the initialisation.*)
  method set_buildings : Building.t list -> unit
					      
  (** Get the buildings owned by the player*)
  method get_buildings : Building.t list
				    
  (** Add a building to the player*)
  method add_building : Building.t -> unit
  (* TODO *)
  method set_unit_hp : Unit.id -> int -> unit
					   
  (** @return the list of entries [log_line, item]  taken by this player.*)
  method get_log : (int * log_item) list


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

  method get_value_resource : int
  method use_resource : int -> bool
  method harvest_buildings_income : unit

end
(**A player is a enhanced logic player. It has a get_next_action that ask for the next action of the player. And it knows the other players on the game.
@param id a logic player can be created with a specific id.*)
class virtual player : ?id:Types.id_player ->  Unit.t list -> Building.t list  ->
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
  method update : Types.update -> unit
  end

(**Create a new player*)
val create_player : unit -> player

(**Create a new dummy player. A dummy player has a predefined set of actions. Useful for tests.*)
val create_dummy_player : Action.t list ->  player
