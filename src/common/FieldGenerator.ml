open Position
open UnionFind

exception NotEnoughSpawns
exception BadSpawnsSequence
exception NotEnoughPlace
exception InvalidPlacement
exception UnitsSuperposition
exception NoPath
exception UnitsSpawnFail
exception StructSpawnFail

(*renvoie la liste des 8 voisins d'une case, <> neighbours, qui renvoie les voisins d'une liste de positions, sans doublons *)
let neighbors m pos =
  List.map (Battlefield.get_tile m)
    (List.filter (Battlefield.in_range m)
      [left (up pos); left pos; left (down pos); up pos; down pos; right(up pos); right pos; right (down pos)]
    )
 (*TODO : use unionfind for neighbours*)
(* add an element to a list without duplication *)
let rec add_elt elt = function
  |[] -> [elt]
  |t::q when t = elt -> t::q 
  |t::q when t > elt -> elt::t::q
  |t::q -> t::(add_elt elt q)

(* check if an element is in a list *)
let rec is_in elt = function
  |[] -> false
  |t::q  -> t = elt || is_in elt q
  
let neighbours l =
  let rec neigh_aux = function
    |[] -> []
    |t::q -> 
      neigh_aux q 
      |> add_elt (up t) 
      |> add_elt (right t)
      |> add_elt (down t)
      |> add_elt (left t)
  in 
  List.filter (fun e -> not (is_in e l)) (neigh_aux l)

let neighbours_corners l =
  let rec neigh_aux = function
    |[] -> []
    |t::q -> 
      neigh_aux q 
      |> add_elt (up t) 
      |> add_elt (right t)
      |> add_elt (down t)
      |> add_elt (left t)
      |> add_elt (up (left t)) 
      |> add_elt (right (up t))
      |> add_elt (down (right t))
      |> add_elt (left (down t))
  in 
  List.filter (fun e -> not (is_in e l)) (neigh_aux l)

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
  let tiles_all = Tile.create_list_from_config () in
  let tiles = List.filter (fun a -> Tile.get_structure a = `Block) tiles_all in
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
    then raise InvalidPlacement

(* teste la connexité *)
let test_path attempt = let (m,_,sp) = attempt in
  if not  (let sp1 = List.hd sp in let dij = Path.dijkstra m sp1 Unit.Walk in List.for_all (fun sp2 -> dij sp2 <> None ) (List.tl sp))
    then raise NoPath

let test_superposed_units attempt = let (m,a,_) = attempt in
  let (w,h) = Battlefield.size m in
  let t = Array.make_matrix w h false in
  List.iter (fun l -> List.iter (fun u -> let (x,y) = topair u#position in if t.(x).(y) then raise UnitsSuperposition else t.(x).(y) <- true) l) a


let init_placement m nbplayers = (* séparé de placement pour ne pas le recalculer en cas de fail de placement *)
  let (width,height) = Battlefield.size m in
  let poslist = ref ([]:Position.t list) in
  let test_dist_edge pos =
    let (a,b) = topair(pos) in a > 10*width/100 && b > 10*height/100 && a < 90*width/100 && b < 90*height/100
  in
  (* trouve les points centraux des armées, (les spawns)*)
  Battlefield.tile_iteri (fun pos ti ->
                          let nei = neighbors m pos in
                          if test_dist_edge pos && Tile.get_name ti = "plain" && count (fun t -> Tile.get_name t = "plain") nei = 8 then
                            poslist := pos:: !poslist
                          ) m;
  if List.length !poslist < nbplayers then raise NotEnoughSpawns else !poslist
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
  | n,[] -> raise BadSpawnsSequence
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
  let place_army_around spawn other_armies_pos=
    let unbound_list = Unit.create_list_from_config() in
    let army = ref [Unit.bind (Unit.create_from_config "general") spawn] in
    let army_pos = ref [spawn] in
    List.iter (fun ui ->
                  for i = 0 to ui#spawn_number - 1 do
                    let ne = List.filter (fun p ->
                                            Battlefield.in_range m p
                                             && (Tile.traversable_m (Battlefield.get_tile m p) ui#movement_type)
                                             && not (List.mem p other_armies_pos)
                                          ) (neighbours !army_pos) in
                    if ne = [] then raise NotEnoughPlace else
                    let r = Random.int (List.length ne) in
                    let pos = List.nth ne r in
                    begin
                      army := (Unit.bind ui pos) :: !army;
                      army_pos := pos :: !army_pos;
                    end
                  done;
            ) unbound_list;
    (!army, (!army_pos)@other_armies_pos)
  in
  let rec placement_armies = function
  | 0 -> (([]:Unit.t list list),([]:Position.t list))
  | n when n > 0 -> let others = placement_armies (n-1) in
                    let ap = place_army_around (List.nth poslist (n-1)) (snd others) in
                    ((fst ap)::(fst others),snd ap)
  | _ -> failwith("generate : nbplayer < 0")
  in
  (fst (placement_armies nbplayers), poslist)

let placement_roads m = m (*TODO*)

let placement_borders m =
  let borders = Tile.create_list_from_config () in
  let placement_border m water beach =
    let poslist_water = Battlefield.tile_filteri (fun pos t ->Tile.get_name t = water && count (fun u -> Tile.get_name u = water) (neighbors m pos) <> 8) m in
    let poslist_beach = Utils.shuffle (List.filter (fun pos -> let t = Battlefield.get_tile m pos in Tile.get_name t <> water && Tile.traversable_m t Unit.Roll) (List.filter (Battlefield.in_range m) (neighbours_corners poslist_water))) in
    let rec behead = function
    | 0,_ -> []
    | n,[] -> raise StructSpawnFail
    | n,p::q -> p::(behead (n-1,q))
    in
    let seeds_beach = behead ((Tile.get_border_rate beach)*(List.length poslist_beach)/1000, poslist_beach ) in
    let grow_border m seed =
      let bord = ref [seed] in
      for i = 1 to Tile.get_border_expansion beach do
      bord := !bord @ (List.filter (fun pos -> Battlefield.in_range m pos && List.mem pos poslist_beach) (neighbours_corners !bord));
      done;
      !bord
    in
    List.iter (fun pos -> Battlefield.set_tile m pos beach) (List.fold_left (fun l e -> (grow_border m e) @ l) [] seeds_beach)
  in
  List.iter (fun beach -> match Tile.get_structure beach with | `Border -> placement_border m (Tile.get_border_name beach) beach | _ -> ()) borders;
  m

let placement_structs m =
  placement_roads (placement_borders m)

let units_spawn m nbplayers nbattempts legit_spawns =
  let rec units_spawn_aux = function
  | 0 -> raise UnitsSpawnFail
  | n ->
    begin
      print_string ("    attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^": ");
      try
        let (a,sp) = placement m nbplayers legit_spawns in
        let attempt = (m,a,sp) in
        (print_string "armies spawned, checking... ";flush_all(); test_movement attempt;test_superposed_units attempt; test_path attempt (* place here any checks on units placement*);print_endline "success"; (a,sp))
      with
      | BadSpawnsSequence -> (print_endline "Not enough spawns found"; units_spawn_aux (n-1) )
      | NotEnoughPlace -> (print_endline " Not enough space around spawn for army"; units_spawn_aux (n-1) )
      | InvalidPlacement -> (print_endline "Unit placed on an area not coresponding to its movement modes"; units_spawn_aux (n-1) )
      | UnitsSuperposition -> (print_endline "Units superposition"; units_spawn_aux (n-1) )
      | NoPath -> (print_endline "No path between armies"; units_spawn_aux (n-1) )
    end
  in (print_endline "  Spawning armies ..."; units_spawn_aux nbattempts)


let structures_spawn m nbplayers nbattempts =
  let rec structures_spawn_aux = function
  | 0 -> raise StructSpawnFail
  | n ->
    begin
      print_string ("    attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^": ");
      try
        let new_m = placement_structs m in
        (print_endline "structures spawn success"(* place here any checks on structures placement*); new_m)
      with
      | StructSpawnFail -> print_newline(); raise StructSpawnFail
    end
  in (print_endline "  Spawning structures ..."; structures_spawn_aux nbattempts)


let generate width height nbplayers nbattempts1 nbattempts2 nbattempts3=
  let rec generate_aux = function
  | 0 -> failwith("generator failed, not enough tries? bad calling arguments?")
  | n ->
    begin
      print_endline ("  attempt "^(string_of_int (nbattempts1 - n +1))^" / "^(string_of_int nbattempts1)^": ");
      try
        let m = structures_spawn (swap_gen width height) nbplayers nbattempts2 in
        let (a,sp) = units_spawn m nbplayers nbattempts3 (init_placement m nbplayers) in
        let attempt = (m,a,sp) in
        (print_endline "Generation success"(* place here any check on map generation*); attempt)
      with
      | StructSpawnFail -> (print_endline "  structures spawn aborted"; generate_aux (n-1) )
      | NotEnoughSpawns -> (print_endline "  Spawning armies ...\n   not enough valid spawns\n  armies spawn aborted"; generate_aux (n-1) )
      | UnitsSpawnFail -> (print_endline "  armies spawn aborted"; generate_aux (n-1) )
    end
  in (print_endline "Generating Battlefield : ";generate_aux nbattempts1)


class t (width:int) (height:int) (nbplayers:int) (generate_attempts:int) (*(structs_attempts:int)*) (units_spawn_attempts:int)=
object (self)
  val g = generate width height nbplayers generate_attempts (*structs_attempts*) 1 units_spawn_attempts
  method field = let m,_,_ = g in m
  method armies = let _,a,_ = g in a
  method spawns = let _,_,sp = g in sp
end

