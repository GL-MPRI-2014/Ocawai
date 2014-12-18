open Player

let log_str output = `Fun(function
  |`String(s) -> Printf.fprintf output "%s" s; `Unit
  | _ -> assert false
)

let log_int output = `Fun(function
  |`Int(i) -> Printf.fprintf output "%i" i; `Unit
  | _ -> assert false
)

class scripted_player (scr : string) (a : Unit.t list) (b : Building.t list) =

  object (self)

  inherit player a b

  val mutable logicPlayerList = []

  val mutable script = Interpreter.empty_script ()

  val mutable output = stdout

  initializer
    output <- open_out (Printf.sprintf "log/player_%i.log" self#get_id)

  method init_script map players =
    script <- ScriptEngine.script_from_file scr
      [("self", `Player(self :> Player.logicPlayer));
       ("players", (`List(List.map (fun p -> `Player(p)) players)));
       ("map", (`Map(map)));
       ("log_int", log_int output);
       ("log_str", log_str output)]
      [("self", `Player_t);
       ("players", `List_t(`Player_t));
       ("map", `Map_t);
       ("selected_unit", `Soldier_t);
       ("selected_pos", `Pair_t(`Int_t, `Int_t));
       ("log_int", `Fun_t(`Int_t, `Unit_t));
       ("log_str", `Fun_t(`String_t, `Unit_t))]

  method get_next_action =
    Thread.delay 0.10;
    try
      let u = Interpreter.main_script script in
      let m = Interpreter.move_script script u in
      try
        let t = Interpreter.attack_script script m u in
        (m, Action.Attack_unit(u,t))
      with
      | ScriptCore.Do_nothing -> (m, Action.Wait)
    with
    | ScriptCore.End_turn -> ([], Action.End_turn)
    | Invalid_argument(s) ->
        print_endline ("Warning, script invalid argument : " ^ s);
        ([], Action.End_turn)


  method update (u:Types.update) =
	()


end
