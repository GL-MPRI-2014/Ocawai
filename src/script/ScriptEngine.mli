(** Main script module. Creates scripts from a file and a global environment*)

val script_from_file : string -> (string * ScriptValues.value) list -> 
  (string * ScriptValues.value_type) list -> Interpreter.script
