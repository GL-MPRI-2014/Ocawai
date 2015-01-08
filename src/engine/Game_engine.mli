class game_engine : unit -> object
    method kill : unit
    method get_players : Player.player list
    method get_neutral_buildings : Building.t list
    method cursor_init_position : Types.id_player -> Position.t
    method init_local : Player.player -> int -> (Player.logicPlayer list*Battlefield.t)
    method init_net : int -> int -> (Player.logicPlayer list*Battlefield.t)
    method run : Mutex.t -> unit
    method is_over : bool
    end


val print_ascii : Battlefield.t -> unit
