open Action

class game_engine () = 
object (self)
  val mutable players = ([||]: Player.player array)
  val mutable field = None
  val mutable map = None
  val mutable map_width = 0
  val mutable map_height = 0
  method get_players =
    Array.tolist players
  method init_local player nbplayers map_wht map_hgt = 
      map_width <- map_wht;
      map_hgt <- map_hgt;
      players <- Array.make nbplayers (Player.create_player ());     
      players.(0) <- player;

      field <- new FieldGenerator.t map_width map_height (self#get_players : Player.player list :> Player.logicPlayer list) 10 5 ;
      map <- field#field;
      (self#get_players :> Player.logic_player list)
  method run = 
    let  current_player = ref (self#init_current_player players_number) and gameover = ref false in
    while not !gameover do
	    let player_turn_end =  ref false and has_played = ref [] in
	    while not (!player_turn_end) do
            let player_turn = players.( List.hd !current_player ) in
		    let next_wanted_action =  player_turn#get_next_action in
		    player_turn_end := ((snd next_wanted_action) = Wait);
		    try
		        let (movement,action) = try_next_action (game#get_players :> logic_player list) (player_turn:> logic_player) !has_played map next_wanted_action in
                if action = End_turn then
                    self#end_turn player_turn_end current_player
                else
                        self#apply_movement player_turn movement has_played
            with
               _ -> self#end_turn player_turn_end current_player;
	    done;
	done


  method private init_current_players nb= 
    let rec aux players_number =
      if players_number = 0 then
		    [0]
	    else
	      (players_number-1)::(init_current_player (players_number -1) )
    in
    aux nb

  method private end_turn player_turn_end current_player =
        player_turn_end := true;
        current_player := ((List.tl !current_player)@([List.hd !current_player]))

  method private apply_movement (player:Player.player) movement has_played =
	    let u = find_unit (List.hd movement) (player :> logic_player) in
     		player#move_unit u movement;
		    has_played := u::!has_played

  method private apply_action player action =
	    match action with
		    | Attack_unit a-> ()
		    | Attack_building a-> ()
                    | _ -> ()





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

