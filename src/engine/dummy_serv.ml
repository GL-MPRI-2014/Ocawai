let () =
    let game = new Game_engine.game_engine () in
    let init = game#init_net 1234 3 100 100 in 
    game#run
