open OUnit2
open Settings_t
open Settings_engine_t

let config = Config.config
let _ =
  config#settings.map_width <- 45;
  config#settings.map_height <- 45;
  config#settings_engine.generate_attempts <- 20;
  config#settings_engine.structs_attempts <- 1;
  config#settings_engine.units_spawn_attempts <- 5

let print_ascii_extended (m:Battlefield.t) (a:Unit.t list list) (p:Path.t) (sp:Position.t list)=
  let (w,h) = Battlefield.size m in
  let str = "??" in
  for j = 0 to h-1 do
    for i = 0 to w-1 do
    let pos = Position.create (i,j) in
    let t = Battlefield.get_tile m pos in
    let name =
      (( if List.mem pos sp
          then "spawn"
        else if List.exists (List.exists (fun u -> u#position = pos)) a
          then "unit"
        else if List.mem pos (Path.get_move p)
          then "path"
        else "") , Tile.get_name t )
    in
    begin
      match fst name with
      | "spawn" -> str.[1] <- 'S'
      | "unit" -> str.[1] <- '@'
      | "path" -> str.[1] <- '#'
      | "" -> str.[1] <- ' '
      | _ -> str.[1] <- '?'
    end;
    begin
    match snd name with
      | "water" | "lake" -> str.[0] <- ' '
      | "shallow" -> str.[0] <- '%'
      | "sand" -> str.[0] <- '~'
      | "beach" | "lake_beach" -> str.[0] <- '_'
      | "road" -> str.[0] <- '='
      | "plain" -> str.[0] <- '.'
      | "forest" -> str.[0] <- ':'
      | "concrete" -> str.[0] <- 'X'
      | "mountain" -> str.[0] <- '/'; if str.[1] = ' ' then str.[1] <- '\\'
      | _ -> str.[0] <- '?'
    end;
    print_string str
    done;
    print_endline ""
  done

let print_ascii m = print_ascii_extended m [[];[]] Path.empty []

let print generator = print_ascii_extended generator#field generator#armies Path.empty []

(*check generation*)
let test_gen test_ctxt =
  let p() = (Player.create_dummy_player [] :>Player.logicPlayer) in
  let rec p_list = function | 0 -> [] | n -> (p())::(p_list (n-1)) in
  config#settings_engine.generation_method <- `Dummy;
  let generator = new FieldGenerator.t (p_list 0) in
  print generator;
  config#settings_engine.generation_method <- `Swap;
  let generator = new FieldGenerator.t (p_list 0) in
  print generator;
  config#settings_engine.generation_method <- `Seeds;
  let generator = new FieldGenerator.t (p_list 0) in
  print generator;
  config#settings_engine.generation_method <- `Island;
  let generator = new FieldGenerator.t (p_list 0) in
  print generator;
  let generator = new FieldGenerator.t (p_list 1) in
  print generator;
  let generator = new FieldGenerator.t (p_list 2) in
  print generator;
  let generator = new FieldGenerator.t (p_list 3) in
  print generator;
  let generator = new FieldGenerator.t (p_list 4) in
  print generator;
  assert_equal () ()

let suite_gen =
  "map generation tests">:::
  ["New map : check dummy">:: test_gen]

let () =
  run_test_tt_main suite_gen
