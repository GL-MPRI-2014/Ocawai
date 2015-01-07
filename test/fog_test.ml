open Fog

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

let print_ascii_extended_fog (m:Battlefield.t) (a:Unit.t list list) (p:Path.t) (sp:Position.t list) fog =
  let (w,h) = Battlefield.size m in
  for j = 0 to h-1 do
    for i = 0 to w-1 do
        if hidden fog (Position.create (i,j)) then
            (print_string "?")
        else
            (
            print_string "u"
            );

    done;
    print_endline ""
  done

let print_ascii m = print_ascii_extended m [[];[]] Path.empty []

let print generator = print_ascii_extended generator#field generator#armies Path.empty []

let print_fog generator fog = print_ascii_extended_fog generator#field generator#armies Path.empty [] fog

let () =


    let p1 = (Player.create_dummy_player [] ) in
    let p2 = (Player.create_dummy_player [] ) in
    let generator = new FieldGenerator.t [(p1 :>Player.logicPlayer);(p2:>Player.logicPlayer)] in
    p1#init generator#field [(p1 :>Player.logicPlayer);(p2:>Player.logicPlayer)];
    p2#init generator#field [(p1 :>Player.logicPlayer);(p2:>Player.logicPlayer)];

    print generator;



    print_fog generator p1#get_fog
