open Action
open Settings_t
open Settings_engine_t

module Log = Log.Make (struct let section = "Engine" end)

let get_opt o =
  match o with
  |Some(s) -> s
  |None -> failwith "Failed to init game engine"

let rec actual_player_list nb_players =
    if nb_players > 0 then
        (actual_player_list (nb_players-1)) @ [nb_players-1]
    else
        []

let rec remove_indice i lst =
    match lst with
    |[] -> []
    |p::q -> if p = i then q else p::(remove_indice i q)


class game_engine () = object (self)
  val mutable players = ([||]: Player.player array)
  val mutable field = None
  val mutable actual_player_l = []
  val mutable is_over = false
  val mutable killed  = false

  method kill = killed <- true

  (* Sends the update to all players *)
  method private notify_all u =
    Array.iter (fun p -> p#update u) players

  method private next_player =
    actual_player_l <- (List.tl actual_player_l) @ [List.hd actual_player_l] ;

    (* Capture buildings before telling him its his turn *)
    self#capture_buildings

  method private actual_player =
    List.hd actual_player_l

  method private remove_player i =
    actual_player_l <- remove_indice i actual_player_l;
    (* Updates of unit/building destruction *)
    Array.iter (fun x ->
      List.iter (fun u ->
        x#update (Types.Delete_unit(u#get_id,(players.(i)#get_id)))
      ) players.(i)#get_army ;
      List.iter (fun b ->
        x#update (Types.Building_changed b) ;
        x#update (Types.Delete_building(b#get_id,(players.(i)#get_id)))
      ) players.(i)#get_buildings ;
    ) players ;
    (* Actually deleting units *)
    List.iter (fun u ->
      players.(i)#delete_unit u#get_id
    ) players.(i)#get_army ;
    (* Actually deleting buildings *)
    List.iter (fun b ->
      b#set_neutral ;
      players.(i)#delete_building (b#get_id)
    ) players.(i)#get_buildings

  method get_players =
    Array.to_list players

  method get_neutral_buildings =
    (get_opt field)#neutral_buildings

  method cursor_init_position =
    Hashtbl.find (get_opt field)#cursor_init_positions

  method is_over = is_over

  method private create_n_scripted =
    (* create one scripted from its id 1..n *)
    let create_one = function
      |2->
        new ScriptedPlayer.scripted_player
          ((Utils.base_path ()) ^ "scripts/olive.script")
      |_->
        new ScriptedPlayer.scripted_player
          ((Utils.base_path ()) ^ "scripts/test.script")
    in
    (* create n scripted calling create_one *)
    let rec create_n = function
      | 0 -> []
      | n -> (create_one n) :: (create_n (n-1))
    in
    create_n


  method init_local player nbplayers =
      let sc_players = self#create_n_scripted (nbplayers - 1) in
      players <-
        Array.init nbplayers (fun n ->
          if n = 0 then player
          else (List.nth sc_players (n-1) :> Player.player)
        );
      field <-
        Some (new FieldGenerator.t
          (self#get_players : Player.player list :> Player.logicPlayer list));
      let players, map =
        ((self#get_players :> Player.logicPlayer list), (get_opt field)#field)
      in
      List.iter (fun p -> p#init_script map players) sc_players;
      actual_player_l <- actual_player_list nbplayers;
      List.iter (fun x -> x#init map players) self#get_players;
      self#notify_players;
      (players, map)

  method init_net port nbplayers =
      let connections = Network_tool.open_n_connections port nbplayers in
      let player_list =
        List.map (fun x -> new NetPlayer.netPlayer x) connections
      in
      players <- (Array.of_list (player_list :> Player.player list));
      field <-
        Some (new FieldGenerator.t
          (self#get_players : Player.player list :> Player.logicPlayer list));
      Array.iter
        (fun p1 ->
          (* On envoie a chaque joueur les var d'initialisation *)
          p1#update (Types.Set_client_player(p1#get_id));
          p1#update
            (Types.Set_logic_player_list
              (List.map (fun x -> x#get_id) player_list));
          p1#update
            (Types.Map
              (Config.config#string_of_battlefield (get_opt field)#field));
          Array.iter
            (fun p2 ->
              (* Pour chaque couple de players (p1,p2), *)
              (* on donne a p1 les updates contenant l'armée *)
              (* et les batiments de p2 *)
              p1#update (Types.Set_army (p2#get_army, p2#get_id));
              p1#update (Types.Set_building (p2#get_buildings, p2#get_id))
            )
            players
        )
        players ;
      actual_player_l <- actual_player_list nbplayers;
      ((self#get_players :> Player.logicPlayer list), (get_opt field)#field)

  method private player_of_unit u =
    let rec aux = function
      |[] -> false
      |t::q -> t#get_id = u#get_id || aux q
    in
    let rec player_aux = function
      |[] -> assert false
      |t::q -> if aux t#get_army then t else player_aux q
    in player_aux self#get_players

  method private is_dead player =
    (*player#get_army = [] (*no more units*)
    ||*) (match player#get_base with
      | None -> true
      | Some b -> b#player_id <> Some (player#get_id) (*base taken*)
    )

  method run : unit =
    Log.infof "One step (%d)..." self#actual_player ;
    let player = players.(self#actual_player) in

    let next_wanted_action =  player#get_next_action in
    begin try
      let next_action = Logics.try_next_action
            (self#get_players :> Player.logicPlayer list)
            (player :> Player.logicPlayer)
            (get_opt field)#field
            next_wanted_action
      in
      match next_action with
      | (_, End_turn) -> self#end_turn
      | (move, Wait) -> self#apply_movement move
      | (move, Attack_unit (u1,u2)) ->
          self#apply_movement move;
          let u1 =
            Logics.unit_of_id u1 (self#get_players :> Player.logicPlayer list)
          in
          let u2 =
            Logics.unit_of_id u2 (self#get_players :> Player.logicPlayer list)
          in
          Logics.apply_attack u1 u2;
          let player_u2 = self#player_of_unit u2 in
          if u2#hp <= 0 then
          (
            player_u2#delete_unit (u2#get_id);
            Array.iter
              (fun x ->
                x#update (Types.Set_unit_hp(u2#get_id,u2#hp,(player_u2#get_id)));
                x#update (Types.Delete_unit (u2#get_id, (player_u2#get_id)))
              )
              players
          )
          else
            Array.iter
              (fun x ->
                x#update (Types.Set_unit_hp(u2#get_id,u2#hp,(player_u2#get_id)))
              )
              players
      | (_, Create_unit (bid,uu)) ->
        let b =
          Logics.building_of_id
            bid
            (self#get_players :> Player.logicPlayer list)
            self#get_neutral_buildings
        in
        if List.exists (fun bb -> bid = bb#get_id) player#get_buildings
        && not (Logics.is_unit_on
                  b#position
                  (self#get_players :> Player.logicPlayer list))
        && player#has_resource uu#price
        then begin
          player#use_resource uu#price;
          player#update (Types.Use_resource uu#price);
          let u = Unit.bind uu b#position player#get_id in
          player#add_unit u;
          self#notify_all (Types.Add_unit(u,(player#get_id))) ;
          self#notify_all
            (Types.Set_unit_played (u#get_id,player#get_id,true)) ;
          u#set_played true
        end
        else raise Bad_create
    with
      |Bad_unit |Bad_path |Bad_attack |Has_played |Bad_create -> self#end_turn
    end;
    if List.length actual_player_l = 2 then
        (
        let enemy_id = (List.hd (List.tl actual_player_l))  in
        if self#is_dead players.(enemy_id) then
            (is_over <- true;
             players.(self#actual_player)#update (Types.You_win);
             players.(enemy_id)#update (Types.Game_over)
            )
        else self#run
        )
    else if List.length actual_player_l = 1 then begin
      is_over <- true;
      players.(self#actual_player)#update (Types.You_win)
    end else if killed then ()
    else self#run

  (* Capture buildings at the beginning of a turn *)
  method private capture_buildings =
    let (changed_buildings,added,removed) =
      Logics.capture_buildings
        (self#get_players :> Player.logicPlayer list)
        (players.(self#actual_player) :> Player.logicPlayer)
        (get_opt field)#buildings
    in
    (*send the list of changed buildings to the players*)
    Array.iter (fun p ->
      List.iter (fun b ->
        p#update (Types.Building_changed (fst b))
      ) changed_buildings
    ) players;
    List.iter
      (fun (b,pid) -> self#notify_all (Types.Add_building (b,pid)))
      added ;
    List.iter
      (fun (bid,pid) -> self#notify_all (Types.Delete_building (bid,pid)))
      removed

  method private end_turn =
    let player = players.(self#actual_player) in
    List.iter
      (fun u ->
        u#set_played false ;
        self#notify_all (Types.Set_unit_played (u#get_id,player#get_id,false))
      )
      player#get_army;

    player#harvest_buildings_income;
    player#update Types.Harvest_income;

    let rec aux lst =
      match lst with
          |[] -> ()
          |p::q ->  (if self#is_dead players.(p) then
                          (
                          self#remove_player p;
                          players.(p)#update (Types.Game_over)
                          );
                    aux q
                      )
    in
    aux actual_player_l;
    if List.length actual_player_l = 1 then
      (* TODO *)
      (* WTF ?! *)
      (* The game should end now, not just say the last player lost *)
      (* players.(self#actual_player)#update (Types.Game_over) *)
      ()
    else (
      (* Enfin, on change de joueur en cours *)
      self#next_player;
      self#notify_players
    )

  method private notify_players = 
    let player = players.(self#actual_player) in 
      
    (* Notify the player *)
    player#update Types.Your_turn;

    (* Notify the others *)
    let pid = player#get_id in
    Array.to_list players
    |> List.filter (fun p -> p <> players.(self#actual_player))
    |> List.iter (fun p -> p#update (Types.Turn_of pid));

  method private apply_movement movement =
    let player = players.(self#actual_player) in
    let u = Logics.find_unit (List.hd movement)
      (player :> Player.logicPlayer) in

    player#move_unit (u#get_id) movement;
    self#notify_all (Types.Set_unit_played (u#get_id,player#get_id,true)) ;
    self#notify_all (Types.Move_unit(u#get_id,movement,(player#get_id))) ;
    u#set_played true

end

type t = game_engine

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
