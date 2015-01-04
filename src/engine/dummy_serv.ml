let () =
    let dummy_mutex = Mutex.create () in
    let game = new Game_engine.game_engine () in
    let _ = game#init_net 1234 3 in 
    game#run dummy_mutex
