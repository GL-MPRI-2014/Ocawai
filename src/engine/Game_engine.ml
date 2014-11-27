open Action
open Settings_t

let get_opt o = 
  match o with
  |Some(s) -> s
  |None -> failwith "Failed to init game engine"

class game_engine () = object (self)
  val mutable players = ([||]: Player.player array)
  val mutable field = None
  val mutable map_width = 0
  val mutable map_height = 0
  val mutable actual_player = 0
  
  val config = new Config.t Config.default_config_files
  
  method get_config = config

  method private next_player = 
    (actual_player + 1) mod (Array.length players)

  method get_players =
    Array.to_list players

  method init_local player nbplayers map_wht map_hgt = 
      config#settings.battlefield_width <- map_wht;
      config#settings.battlefield_height <- map_hgt;
      players <- Array.init nbplayers (fun n -> if n = 0 then player else Player.create_player ());     
      field <- Some (new FieldGenerator.t (self#get_players : Player.player list :> Player.logicPlayer list) config);
      ((self#get_players :> Player.logicPlayer list), (get_opt field)#field)

  method init_net port nbplayers map_wht map_hgt = 
      config#settings.battlefield_width <- map_wht;
      config#settings.battlefield_height <- map_hgt;
      let connections = Network_tool.open_n_connections port nbplayers in
      let player_list = List.map (fun x -> NetPlayer.create_netPlayer x [] [] ) connections in
      players <- (Array.of_list (player_list :> Player.player list));     
      field <- Some (new FieldGenerator.t (self#get_players : Player.player list :> Player.logicPlayer list) config);
      ((self#get_players :> Player.logicPlayer list), (get_opt field)#field)

  method private player_of_unit u = 
    let rec aux = function
      |[] -> false
      |t::q -> t#id = u#id || aux q
    in
    let rec player_aux = function
      |[] -> assert false
      |t::q -> if aux t#get_army then t else player_aux q
    in player_aux self#get_players

  method run : unit = 
    let player = players.(actual_player) in
    let next_wanted_action =  player#get_next_action in
    begin try
      let next_action = Logics.try_next_action
          (self#get_players :> Player.logicPlayer list)
          (player :> Player.logicPlayer)
          (get_opt field)#field 
          next_wanted_action 
      in
      match next_action with
      |(_, End_turn) -> self#end_turn
      |(move, Wait ) -> self#apply_movement move
      |(move, Attack_unit (u1,u2)) -> 
          self#apply_movement move;
          Logics.apply_attack u1 u2;
          if u2#hp <= 0 then 
            (
            (self#player_of_unit u2)#delete_unit u2;
            Array.iter (fun x -> x#update (Types.Delete_unit(u2,(x#get_id))) ) players
            )
      |(move, _) -> self#apply_movement move
    with
      |Bad_unit |Bad_path |Bad_attack |Has_played -> self#end_turn 
    end; 
    if true (* test gameover here *) then self#run

  method private end_turn =
    let player = players.(actual_player) in
    List.iter (fun u -> u#set_played false) player#get_army;
    actual_player <- self#next_player

  method private apply_movement movement =
    let player = players.(actual_player) in
    let u = Logics.find_unit (List.hd movement) 
      (player :> Player.logicPlayer) in
    player#move_unit u movement;
    Array.iter (fun x -> x#update (Types.Move_unit(u,movement,(x#get_id))) ) players; 
    u#set_played true
end


type t = game_engine


let print_ascii_extended (m:Battlefield.t) (a:Unit.t list list) (p:Path.t) (sp:Position.t list)=
  let (w,h) = Battlefield.size m in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
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
    print_string (let str = "??" in
      begin
        match fst name with
        | "spawn" -> str.[1] <- 'S';
        | "unit" -> str.[1] <- '@';
        | "path" -> str.[1] <- '#';
        | "" -> str.[1] <- ' ';
        | _ -> ()
      end;
      begin
      match snd name with
        | "water" -> str.[0] <- ' ';
        | "shallow" -> str.[0] <- '%';
        | "sand" -> str.[0] <- '~';
        | "beach" -> str.[0] <- '_';
        | "road" -> str.[0] <- '=';
        | "plain" -> str.[0] <- '.';
        | "forest" -> str.[0] <- ':';
        | "concrete" -> str.[0] <- 'X';
        | "mountain" -> str.[0] <- '/'; if str.[1] = ' ' then str.[1] <- '\\';
        | _ -> ()
      end;
      str
    )
    done;
    print_endline ""
  done

let print_ascii m = print_ascii_extended m [[];[]] Path.empty []

let get_game_parameters ()=
	print_string "Entrer le nom de la partie : ";
  let game_name = read_line () in
	print_string "Entrer le nombre de joueur : ";
	let players_number = read_int ()  in
	print_string "Entrer la hauteur de la carte : ";
	let map_width = read_int () in
	print_string "Entrer la largeur de la carte: ";
	let map_height = read_int () in
	(game_name,players_number,map_width,map_height)

let dijkstra_test gen =
  let dij = Path.dijkstra (gen#field) (List.hd gen#spawns) (Unit.Walk) in
  let r = dij (List.nth gen#spawns 1) in
  print_ascii_extended gen#field gen#armies (match r with | None -> Path.empty | Some (_,b) -> b) gen#spawns;
  print_endline ("path length between spawns 1 and 2 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a))

