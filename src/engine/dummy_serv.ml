let () =
    let game = new Game_engine.game_engine () in
    game#init_local (Player.create_player ()) 4 100 100;
    game#run
