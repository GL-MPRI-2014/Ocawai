open Player

class scripted_player (scr : string) (a : Unit.t list) (b : Building.t list) = 

  object (self) 

  inherit player a b

  val mutable logicPlayerList = [] 

  val mutable script = ScriptEngine.script_from_file scr

  method get_next_action = 
    Thread.delay 0.50;
    ScriptValues.expose (`Player(self :> Player.logicPlayer)) `Player_t "self";
    try 
      let u = Interpreter.main_script script in
      let m = Interpreter.move_script script u in
      (m, Action.Wait)
    with
    | _ -> ([], Action.End_turn)

  method set_logicPlayerList playersList =
	()

  method get_logicPlayerList =
	logicPlayerList

end
