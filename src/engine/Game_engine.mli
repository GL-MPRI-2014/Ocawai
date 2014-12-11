class game_engine : unit -> object
    method get_players : Player.player list
    method get_neutral_buildings : Building.t list
    method init_local : Player.player -> int -> (Player.logicPlayer list*Battlefield.t)
    method init_net : int -> int -> (Player.logicPlayer list*Battlefield.t)
    method run : unit
    end


val print_ascii : Battlefield.t -> unit
