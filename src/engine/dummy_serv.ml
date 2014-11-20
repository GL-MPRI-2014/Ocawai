let () =
    let game = new Game_engine.game_engine () in
    let init = game#init_local (Player.create_player ()) 4 100 100 in 
    game#run
