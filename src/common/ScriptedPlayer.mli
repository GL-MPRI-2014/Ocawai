class scripted_player : ?id:Types.id_player -> string ->
object

  inherit Player.player 
	    
  method init_script : Battlefield.t -> (Player.logicPlayer list) -> unit
								       
  method get_next_action : Mutex.t -> Action.t


  method update : Types.update -> unit

end
