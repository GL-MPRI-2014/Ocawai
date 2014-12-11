class scripted_player : string -> Unit.t list -> Building.t list ->
object

  inherit Player.player
	    
  method init_script : Battlefield.t -> (Player.logicPlayer list) -> unit
								       
  method get_next_action : Action.t

end
