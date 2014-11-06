(** main function for testing the engine (only the generation and djikstra for now) *)
let print_ascii m a p sp=
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
  print_string (match name with
              | "","water" -> "  "
              | "","plain" -> ". "
              | "","forest" -> ": "
              | "","concrete" -> "= "
              | "","mountain" -> "/\\"
              | "spawn","water" -> " S"
              | "spawn","plain" -> ".S"
              | "spawn","forest" -> ":S"
              | "spawn","concrete" -> "=S"
              | "spawn","mountain" -> "/S"
              | "unit","water" -> " @"
              | "unit","plain" -> ".@"
              | "unit","forest" -> ":@"
              | "unit","concrete" -> "=@"
              | "unit","mountain" -> "/@"
              | "path","water" -> " #"
              | "path","plain" -> ".#"
              | "path","forest" -> ":#"
              | "path","concrete" -> "=#"
              | "path","mountain" -> "/#"
              | _ -> "?"
  )
  done;
  print_endline ""
done

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

let djikstra_test gen =
  let dij = Path.dijkstra (gen#field) (List.hd gen#spawns) (Unit.Walk) in
  let r = dij (List.nth gen#spawns 1) in
  print_ascii gen#field gen#armies (match r with | None -> Path.empty | Some (_,b) -> b) gen#spawns;
  print_endline ("path length between spawns 1 and 2 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a))
  
let init_players list_armies =
	let armies = ref list_armies in 
	let players = Array.make (List.length list_armies) (Player.create_player ()) in
	for i=0 to ((List.length list_armies) -1) do
  	players.(i)#set_army (List.hd !armies);
    armies := (List.tl !armies);
  done;
  players

let rec init_current_player players_number = 
  if players_number = 0 then 
		[0]
	else
	  (players_number-1)::(init_current_player (players_number -1) )

let () =
begin
	let (game_name,players_number,map_width,map_height) = get_game_parameters () in  
  let init_field = new FieldGenerator.t map_width map_height players_number 10 5 in
 (* djikstra_test init_field; *)
  let players = init_players (init_field#armies) and current_player = ref (init_current_player players_number) and gameover = ref false in
  while not !gameover do
		let player_turn_end =  ref false and has_played = [] in
		while not (!player_turn_end) do
			let next_wanted_action =  players.( List.hd !current_player )#get_next_action in
			player_turn_end := ((snd next_wanted_action) = Action.Wait);

			(*let action = Logics.try_next_action players (List.hd current_player) next_wanted_action in *)
     (* apply_action *)
      ()
		done;
	done;
end
