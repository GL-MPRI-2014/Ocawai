open Position

exception NotEnoughSpawns
exception NotEnoughPlace
exception InvalidPlacement
exception NoPath
exception UnitsSpawnFail

(*renvoie la liste des 8 voisins d'une case, <> neighbours de Position, qui ne renvoie pas les diagonales *)
let neighbors m pos =
  List.map (Battlefield.get_tile m)
    (List.filter (Battlefield.in_range m)
      [left (up pos); left pos; left (down pos); up pos; down pos; right(up pos); right pos; right (down pos)]
    )

let rec count f l = List.fold_left (fun c e -> if f e then 1+c else c) 0 l

(* functions working with densities *)
(* compute the total density of a list of tiles *)
let total_density tiles =
  List.fold_left (+) 0 (List.map Tile.get_density tiles)

(* return a random tile with regards to densities *)
let get_tile_with_density total tiles =
  let d = (Random.int total) + 1 in
  let rec nth_dens n = function
  | p::q when Tile.get_density p < n -> nth_dens (n - Tile.get_density p) q
  | p::q -> p
  | _ -> assert false
  in
  nth_dens d tiles

(* others useful functions *)
(* degre de contiguite d'une position = nb de voisins identiques / nb de voisins*)
let contiguite m pos =
  let name = Tile.get_name (Battlefield.get_tile m pos) in
  let nei = neighbors m pos in
  float_of_int (count (fun t -> Tile.get_name t = name) nei) /. float_of_int (List.length nei)


let swap_gen width height = (* stats sur 300 générations : en 100*100 a 2 joueurs, la génération prend en moyenne 1.2 secondes et rate 1 fois sur 3 *)
  Random.self_init();
  let tiles = Tile.create_list_from_config () in
  let get_tile_with_density () =
    let total = total_density tiles in
    get_tile_with_density total tiles in
  let m = Battlefield.create width height (List.hd tiles) in

  (* fill the map with regards to densities *)
  Battlefield.tile_iteri (fun p _ -> Battlefield.set_tile m p (get_tile_with_density ())) m;

  let swap pos1 pos2 =
    let t1 = Battlefield.get_tile m pos1 in
    Battlefield.set_tile m pos1 (Battlefield.get_tile m pos2);
    Battlefield.set_tile m pos2 t1
  in

  (* on prends 2 positions random, on les swap si ca augmente la contiguite, et on itere*)
  for i = 0 to 50 * width * height (* arbitraire *) do
    let pos1 = create (Random.int width , Random.int height) in
    let pos2 = create (Random.int width , Random.int height) in
    let previous = contiguite m pos1 +. contiguite m pos2 in
    swap pos1 pos2;
    if previous > contiguite m pos1 +. contiguite m pos2 then
      swap pos1 pos2;
  done;
  m

(* un placement est valide si les unites sont placees sur des tuiles acceptant leurs mouvements *)
let test_movement attempt = let (m,a,_) = attempt in
  if not (List.for_all (List.for_all (fun u -> Tile.traversable (Battlefield.get_tile m (u#position)) u)) a)
    then raise InvalidPlacement else ()

(* teste la connexité *)
let test_path attempt = let (m,_,sp) = attempt in
  if not  (let sp1 = List.hd sp in let dij = Path.dijkstra m sp1 Unit.Walk in List.for_all (fun sp2 -> dij sp2 <> None ) (List.tl sp))
    then raise NoPath


let init_placement m nbplayers = (* séparé de placement pour ne pas le recalculer en cas de fail de placement *)
  let (width,height) = Battlefield.size m in
  let poslist = ref ([]:Position.t list) in
  let test_dist_edge pos =
    let (a,b) = Position.topair(pos) in a > 10*width/100 && b > 10*height/100 && a < 90*width/100 && b < 90*height/100
  in
  (* trouve les points centraux des armées, (les spawns)*)
  Battlefield.tile_iteri (fun pos ti ->
                          let nei = neighbors m pos in
                          if test_dist_edge pos && Tile.get_name ti = "plain" && count (fun t -> Tile.get_name t = "plain") nei = 8 then
                            poslist := pos:: !poslist
                          ) m;
  if !poslist = [] then raise UnitsSpawnFail else !poslist
  (*begin (* précalcul isolant la composante connexe maximale de poslist, permet d'éviter des fail de placement d'armées. 
            Après test, pas rentable, les fails ne pénalisent pas beaucoup et le précalcul coute cher (1-2 secondes en 100*100) *)
    let le = List.length !poslist in
    let connected = Array.init le (fun i -> i) in
    let rec find_first n nn = function
    | _ when nn >= le -> ()
    | [] -> find_first 0 (nn+1) !poslist
    | p::q when connected.(n) = nn ->
      begin
        let dij = Path.dijkstra m p Unit.Walk in
        List.iteri (fun i v -> connected.(i) <- nn) (List.filter (fun sp2 -> dij sp2 <> None ) !poslist);
        find_first 0 (nn+1) !poslist
      end
    | p::q -> find_first (n+1) nn q
    in
    find_first 0 0 !poslist;
    let scores = Array.make le 0 in
    Array.iter (fun e -> scores.(e) <- succ scores.(e)) connected;
    let max_comp = ref (0,scores.(0)) in
    for i=1 to le-1 do
      if scores.(i) > (snd !max_comp) then
        max_comp := (i,scores.(i));
    done;
    List.map snd (List.filter fst (List.mapi (fun i p -> (connected.(i) = fst !max_comp , p) ) (!poslist)))
  end*)

(* place nbplayers sur la map m *)
let placement m nbplayers legit_spawns =
  let (width,height) = Battlefield.size m in
  let rec behead = function
  | 0,_ -> []
  | n,[] -> raise NotEnoughSpawns
  | n,p::q -> p::(behead (n-1,q))
  in
  (* vaut true ssi p est a une certaine distance de toutes les positions dans une liste*)
  let rec test_dist_spawns p = function
    | [] -> true
    | p1::q -> (Position.dist p p1 > (90*width + 90*height)/(100*nbplayers)) && test_dist_spawns p q
  in
  let filtered_pos = ref [] in
  List.iter (fun pos -> if test_dist_spawns pos !filtered_pos then filtered_pos := pos :: !filtered_pos) (Utils.shuffle legit_spawns);
  let poslist = behead (nbplayers, Utils.shuffle !filtered_pos) in
  (* check la connexite de l'ensemble de spawns selectionnés *)
  test_path (m,(),poslist);

  (* positionne une armée autours de la position spawn*)
  let place_army_around spawn =
    let ui_list = Ag_util.Json.from_file Unit_j.read_t_list "resources/config/units.json" in
    let army = ref [Unit.create_from_config "general" spawn] in
    let army_pos = ref [spawn] in
    List.iter (fun ui ->
                  for i = 0 to ui.Unit_t.spawn_number - 1 do
                    let ne = List.filter (fun p ->
                                            Battlefield.in_range m p
                                             && (Tile.traversable_m (Battlefield.get_tile m p) Unit.Walk)
                                          ) (neighbours !army_pos) in
                    if ne = [] then raise NotEnoughPlace else
                    let r = Random.int (List.length ne) in
                    let pos = List.nth ne r in
                    begin
                      army := (Unit.create_from_unit_t ui pos) :: !army;
                      army_pos := pos :: !army_pos;
                    end
                  done;
            ) ui_list;
    !army
  in
  let rec placement_armies = function
  | 0 -> ([[]]:Unit.t list list) (* a remplacer par ([]:Unit.t list list) pour separer les armees *)
  | n when n > 0 -> let others = placement_armies (n-1) in
                    [(place_army_around (List.nth poslist (n-1)))@(List.hd others)]
                    (* fusionne toutes les armes au player 1, a remplacer par (place_army_around (List.nth poslist (n-1)))::others *)
  | _ -> failwith("generate : nbplayer < 0")
  in
  (placement_armies nbplayers, poslist)


let units_spawn m nbplayers nbattempts legit_spawns=
  let rec units_spawn_aux = function
  | 0 -> raise UnitsSpawnFail
  | n ->
    begin
      print_endline ("    attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^" ...");
      try
        let (a,sp) = placement m nbplayers legit_spawns in
        let attempt = (m,a,sp) in
        (print_string "|"; flush_all (); test_movement attempt; test_path attempt (* place here any checks on units placement*); (a,sp))
      with
      | NotEnoughSpawns -> (print_endline " Not enough spawns found"; units_spawn_aux (n-1) )
      | NotEnoughPlace -> (print_endline " Not enough space around spawn for army"; units_spawn_aux (n-1) )
      | InvalidPlacement -> (print_endline " Unit placed on an area not coresponding to its movement modes"; units_spawn_aux (n-1) )
      | NoPath -> (print_endline " No path between armies"; units_spawn_aux (n-1) )
    end
  in (print_endline "  Spawning armies ..."; units_spawn_aux nbattempts)


let generate width height nbplayers nbattempts1 nbattempts2 =
  let rec generate_aux = function
  | 0 -> failwith("generator failed, try more attempts")
  | n ->
    begin
      print_string ("  attempt "^(string_of_int (nbattempts1 - n +1))^" / "^(string_of_int nbattempts1)^" ");
      try
        let attempt =
          let m = swap_gen width height in
          let (a,sp) = units_spawn m nbplayers nbattempts2 (init_placement m nbplayers) in
            (m,a,sp)
        in ( print_endline "|"(* place here any check on map generation*); attempt)
      with
      | UnitsSpawnFail -> (print_endline " units spawn aborted"; generate_aux (n-1) )
    end
  in (print_endline "Generating Battlefield, please wait ... ";generate_aux nbattempts1)


class t (width:int) (height:int) (nbplayers:int) (generate_attempts:int) (units_spawn_attempts:int)=
object (self)
  val g = generate width height nbplayers generate_attempts units_spawn_attempts
  method field = let m,_,_ = g in m
  method armies = let _,a,_ = g in a
  method spawns = let _,_,sp = g in sp
end

