class game_engine : unit -> object
    method config : Config.t
    method get_players : Player.player list
    method init_local : Player.player -> int -> int -> int -> (Player.logicPlayer list*Battlefield.t)
    method run : unit
    end
