class game_engine (a : Player.player list) = 
object (self)
  val mutable players = (a : Player.player list)
  method get_players = players
end

type t = game_engine

let create_game_engine (n:int) = 
  (*let socket_list =  Network_tool.open_n_connexions 2000 n in*)
  let res = ref [] in
  for i = 1 to n do
    res := (Player.create_player ())::!res
  done;
  new game_engine !res

 
