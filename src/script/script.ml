let () =
  let scr = ScriptEngine.script_from_file "src/script/test.script" in
  let pos = Position.topair (Interpreter.main_script scr) in
  Printf.printf "\nPosition selected : (%i,%i)\n" (fst pos) (snd pos)
