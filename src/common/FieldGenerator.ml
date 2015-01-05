open Position
open Utils
open Settings_t
open Settings_engine_t

exception GeneratorFailure

exception NotEnoughSpawns
exception BadSpawnsSequence
exception NotEnoughPlace
exception InvalidPositioning
exception UnitsSuperposition
exception NoPath
exception UnitsSpawnFail
exception StructSpawnFail

module GenLog = Log.Make (struct let section = "Generation" end)

(* functions used to get the 4 or 8 direct neighbors of a position,
  as a position list or a tile list *)

let neigh_list t = [up t;down t;left t;right t]
let neigh_list_corners t = [up t;down t;left t;right t;up (left t);down (right t);left (down t);right (up t)]

let pos_neighbors_aux m f p =
  List.filter
    (Battlefield.in_range m)
    (f p)

let pos_neighbors_corners m p =
  pos_neighbors_aux m neigh_list_corners p

let pos_neighbors m p =
  pos_neighbors_aux m neigh_list p

let pos_eucl_disk m p d =
  pos_neighbors_aux m (fun t -> get_eucl_disk t d) p

let neighbors_corners m pos =
  List.map
    (Battlefield.get_tile m)
    (pos_neighbors_corners m pos)

let neighbors m pos =
  List.map
    (Battlefield.get_tile m)
    (pos_neighbors m pos)

let eucl_disk m p d =
  pos_neighbors_aux m (fun t -> get_eucl_disk t d) p

(* function used to get the neighbors of a position list not already in the entry list, and without duplications *)

module SetPos = Set.Make(Position)

let rec neighbours_set_aux f = function
| [] -> SetPos.empty
| p::q -> List.fold_right SetPos.add (f p) (neighbours_set_aux f q)

let neighbours_set l =
  SetPos.filter
    (fun e -> not (List.mem e l))
    (neighbours_set_aux neigh_list l)

let neighbours_corners_set l =
  SetPos.filter
    (fun e -> not (List.mem e l))
    (neighbours_set_aux neigh_list_corners l)

let neighbours l =
  SetPos.fold
    (fun e l -> e::l)
    (neighbours_set l)
    []

let neighbours_corners l =
  SetPos.fold
    (fun e l -> e::l)
    (neighbours_corners_set l)
    []

(* general utility functions *)

let count f l = List.fold_left (fun c e -> if f e then 1+c else c) 0 l

let matrix_init sx sy f =
  Array.init
    sx
    (fun x ->
      Array.init
      sy
      (f x)
    )

let matrix_foreach f =
  Array.iteri (fun x -> Array.iteri (f x))


(* return the list of list of positions of connected composants among positions
   that satifies f *)
let find_connected_composants f m =
  let sizex, sizey = Battlefield.size m in
  (* singletons *)
  let sings =
    matrix_init
      sizex
      sizey
      (fun x y ->
        let p = Position.create (x, y) in
        if f p then
          Some(UnionFind.make_sing p)
        else
          None
      ) in
  let union a (x',y') =
    sings.(x').(y') >? (UnionFind.union_gen a) in
  (* union when neighbours are walkables *)
  matrix_foreach
    (fun x y aopt ->
      aopt >? (fun a ->
        List.iter
        (fun b ->
          if Battlefield.in_range m b then
            let x', y' = topair b in
            union a (x', y')
        )
        (neighbours [Position.create (x,y)]))
    )
    sings;
  List.map
    UnionFind.get_data
    (
      List.map
        (function Some(r) -> r | None -> assert false)
      (List.filter
        (function
          | None -> false
          | Some(r) -> UnionFind.is_representative r)
        (List.concat (Array.to_list (Array.map Array.to_list sings))))
    )




(* functions working with densities *)
(* compute the total density of a list of tiles *)
let total_density tiles =
  List.fold_left (+) 0 (List.map Tile.get_density tiles)

(* return a random tile with regards to densities *)
let get_tile_with_density total tiles =
  let d = (Random.int total) + 1 in
  let rec nth_dens n = function
  | p::q when Tile.get_density p < n ->
      nth_dens (n - Tile.get_density p) q
  | p::q -> p
  | _ -> assert false
  in
  nth_dens d tiles

(* the contiguity of a position is defined as its number of identical neighbors on its total number of neighbors *)
let contiguity m pos name =
  let nei = neighbors_corners m pos in
  let a = count (fun t -> Tile.get_name t = name) nei in
  let b = List.length nei in
  float_of_int a /. float_of_int b


(* smooth a map by swaping random tiles iff the swap increase the local contigity *)
let swap_smoothing m factor is_seed =
  let (width,height) = Battlefield.size m in
  let swap pos1 pos2 =
    let t1 = Battlefield.get_tile m pos1 in
    Battlefield.set_tile m pos1 (Battlefield.get_tile m pos2);
    Battlefield.set_tile m pos2 t1
  in
  for i = 0 to factor * width * height do
    let pos1 = create (Random.int width , Random.int height) in
    let pos2 = create (Random.int width , Random.int height) in
    let name1 = Tile.get_name (Battlefield.get_tile m pos1) in
    let name2 = Tile.get_name (Battlefield.get_tile m pos2) in
    if(name1 <> name2) then
    begin
      let pc1 = contiguity m pos1 name1 in
      let pc2 = contiguity m pos2 name2 in
      let c1 = contiguity m pos1 name2 in
      let c2 = contiguity m pos2 name1 in
      (* we do the swap if the contiguity has increased
        and if we are not looking at a seed tile
        (they can be rendered to examine how seed_gen works, we don't want to move them ) *)
      if pc1 +. pc2 < c1 +. c2  && (not (is_seed m pos1)) && (not (is_seed m pos2)) then
        swap pos1 pos2;
    end
  done

(* count neighbors to find the most occuring one, and set it at pos *)
let smooth m tiles pos is_seed =
  let nei = neighbors_corners m pos in
  let nb = Array.make (List.length tiles) 0 in
  let rec find_n e = function
  | [],_ -> assert false
  | p::q,n when Tile.get_name p = Tile.get_name e -> n
  | p::q,n -> find_n e (q,n+1)
  in
  let rec count_nb = function
  | [] -> ()
  | p::q ->
    let n = find_n p (tiles,0) in
    nb.(n) <- nb.(n) + 1;
    count_nb q
  in
  count_nb nei;
  let ma = ref (-1) in
  let ma_n = ref (-1) in
  for i = 0 to List.length tiles - 1 do
    if !ma < nb.(i) then (ma := nb.(i);ma_n:=i)
  done;
  let t = List.nth tiles !ma_n in
  Battlefield.set_tile m pos t


(* remove isolated tiles (isolated is defined by contiguity <= range)
  by changing them to their most occuring neighbor *)
let hard_smoothing m tiles range is_seed =
  (* to be changed position list *)
  let to_smooth =
    Battlefield.tile_filteri
    (fun p t ->
      (not (is_seed m p))
      && contiguity m p (Tile.get_name (Battlefield.get_tile m p)) <= range
    )
    m
  in
  List.iter (fun pos -> smooth m tiles pos is_seed) to_smooth

(* does the same but there is only a chance (1- (proba/100)*(contiguity/range)) to smooth a position*)
let random_hard_smoothing m tiles range proba is_seed =
  (* to be changed position list *)
  let to_smooth =
    Battlefield.tile_filteri
    (fun p t ->
      (not (is_seed m p))
      && let a =
            (1. -. (float_of_int proba *. contiguity m p (Tile.get_name (Battlefield.get_tile m p))) /. (100. *. range) )
         in
         let r = Random.float 1. in
         r<a
    )
    m
  in
  List.iter (fun pos -> smooth m tiles pos is_seed) to_smooth

let dummy_gen name =
  let tile = Config.config#tile name in
  let width = Config.config#settings.map_width in
  let height = Config.config#settings.map_height in
  Battlefield.create width height tile

(* generate a map randomly then uses swap_smoothing *)
let swap_gen () =
  (* special tiles used by the generation *)
  let is_seed m p = Tile.get_name (Battlefield.get_tile m p) = "seed" in
  let tiles_all = Config.config#tiles_list in
  let tiles =
    List.filter
    (fun a -> Tile.get_structure a = `Block)
    tiles_all in
  if (List.length tiles = 0) then failwith("no `Block tiles in") else
  let total = total_density tiles in
  let m = dummy_gen "blank" in

  (* fill the map with regards to densities *)
  Battlefield.tile_iteri
    (fun p _ -> Battlefield.set_tile m p (get_tile_with_density total tiles))
    m;
  swap_smoothing m 50 is_seed;
  (* change tiles with less than two identical neighbors *)
  hard_smoothing m tiles (2./.8.) is_seed;
  m

(* grow seed t on nb_grow tiles where is_blank holds of its neighbors position set setpos*)
let grow_seed m t nb_grow setpos background_name =
  let is_background m p = Tile.get_name (Battlefield.get_tile m p) = background_name in
  (* extract at most n elements of a list*)
  let rec behead = function
  | 0,_ -> []
  | n,_ when n<0 -> assert false
  | n,[] -> []
  | n,p::q -> p::(behead (n-1,q))
  in
  let sl = behead (nb_grow,Utils.shuffle (SetPos.fold (fun e l -> e::l) setpos [])) in
  List.iter (fun p -> if is_background m p then Battlefield.set_tile m p t) sl;
  SetPos.filter
    (fun p -> is_background m p)
    (SetPos.union
      (SetPos.filter
        (fun p -> Battlefield.in_range m p)
        (neighbours_set sl)
      )
      setpos
    )

(* place seeds (generation centers) on blank tiles, and expand them until the whole map is filled *)
let seed_fill m =
  (* special tiles used by the generation *)
  let is_blank m p = Tile.get_name (Battlefield.get_tile m p) = "blank" in
  let is_seed m p = Tile.get_name (Battlefield.get_tile m p) = "seed" in
  let width = Config.config#settings.map_width in
  let height = Config.config#settings.map_height in
  let nb_blank = width*height in(*
    count 
      (fun _ -> true)
      (Battlefield.tile_filter (fun t -> Tile.get_name t = "blank") m)
  in*)
  let nbseeds = nb_blank / Config.config#settings_engine.nb_seeds_ratio in
  let dist_min_btw_seeds = Config.config#settings_engine.dist_min_between_seeds in
  let tiles_all = Config.config#tiles_list in
  let tiles =
    List.filter
    (fun a -> Tile.get_structure a = `Block)
    tiles_all in
  if (List.length tiles = 0) then failwith("no `Block tiles in") else
  let total = total_density tiles in
  (* initial seeds creation *)
  let rec create_seeds = function
  | 0 -> []
  | n ->
    (* pick a random tile with regards to densities *)
    let t = get_tile_with_density total tiles in
    (* pick a random position and check that its not too close to other different seeds*)
    let p = create(Random.int width,Random.int height) in
    if List.for_all
        (fun pos ->
          Battlefield.in_range m pos
          && ( is_blank m pos
              || Tile.get_name (Battlefield.get_tile m p) = Tile.get_name t (* remove this line if using
                                            (Config.config#tile "seed") instead of t on the following statement*)
             )
        )
        (eucl_disk m p dist_min_btw_seeds)
    then
    (
      Battlefield.set_tile m p t; (* option : replace t by (Config.config#tile "seed") in order to visualize the seeds on the map
                                     note : the previous test is affected, use with caution *)
      (t,p)::(create_seeds (n-1))
    )
    else
      create_seeds (n-1)
  in
  (* grow every seed simultaneously, remove the ones that can't be expanded, until no seed can grow*)
  let rec grow = function
  | [] -> ()
  | ll ->
    grow (
        List.filter
          (fun (t,l) -> l <> SetPos.empty )
          (
            List.map
            (fun (t,s) ->
              (
                t, grow_seed m t (Tile.get_grow_speed t) s "blank"
              )
            )
            ll
          )
        )
  in
  (* initial seeds *)
  let seeds = create_seeds nbseeds in
  (* associates their neighbors position set to them *)
  let neigh =
    List.map
      (fun (t,p) ->
        (t, SetPos.filter
              (fun pos ->
                Battlefield.in_range m pos
                && is_blank m pos
              )
              (neighbours_set [p])
        )
      )
      seeds
  in
  grow neigh;
  (* change tiles with less than two identical neighbors *)
  random_hard_smoothing m tiles (2./.8.) 50 is_seed;
  hard_smoothing m tiles (1./.8.) is_seed;
  hard_smoothing m tiles (1./.8.) is_seed;
  m

(* place seeds (generation centers) and expand them until the whole map is filled *)
let seeds_gen () =
  let m = dummy_gen "blank" in
  seed_fill m

let seeds_island () =
  (* special tiles used by the generation *)
  let blank_tile = Config.config#tile "blank" in
  let is_water m p = Tile.get_name (Battlefield.get_tile m p) = "water" in
  let width = Config.config#settings.map_width in
  let height = Config.config#settings.map_height in
  let m = dummy_gen "water" in
  let seed_pos = create(width/2,height/2) in
  Battlefield.set_tile m seed_pos blank_tile;
  (* grow every seed simultaneously, remove the ones that can't be expanded, until no seed can grow*)
  let rec grow = function
  | l,n when l = SetPos.empty || n<=0 -> ()
  | l,n ->
    grow ((grow_seed m blank_tile (max 1 (Tile.get_grow_speed blank_tile)) l "water"),n-1)
  in
  let neigh =
    SetPos.filter
      (fun pos ->
        Battlefield.in_range m pos
        && is_water m pos
      )
      (neighbours_set [seed_pos])
  in
  grow (neigh,width*height*Config.config#settings_engine.island_expansion/100);
  seed_fill m


(* armies are in valid positions only if their units are placed on tiles where they can move *)
let check_movement attempt =
  let (m,a,_) = attempt in
  let b =
    List.for_all
      (List.for_all
        (fun u -> Tile.traversable (Battlefield.get_tile m (u#position))
        u)
      )
      a in
  if not b then
    raise InvalidPositioning

(* test connexity between armies *)
let check_path attempt =
  let (m,_,sp) = attempt in
  (*let sp = List.map (fun b -> b#position) (List.filter (fun b -> b#name = "base") bl) in*)
  if List.length sp > 0 then
  let sp1 = List.hd sp in
  let dij = Path.dijkstra m sp1 Unit.Tread in
  let b =
    List.for_all
      (fun sp2 -> dij sp2 <> None )
      (List.tl sp) in
  if not b then
    raise NoPath

(* test that there is at most one unit per position *)
let check_superposed_units attempt = let (m,a,_) = attempt in
  let (w,h) = Battlefield.size m in
  let t = Array.make_matrix w h false in
  List.iter
    (fun l ->
      List.iter
        (fun u ->
          let (x,y) = topair u#position in
          if t.(x).(y) then
            raise UnitsSuperposition
          else
            t.(x).(y) <- true
        )
        l
    )
    a

(* head of the positioning function, separated to avoid calculing it multiple times if positioning fails *)
let init_positioning m nbplayers =
  let (width,height) = Battlefield.size m in
  let poslist = ref ([]:Position.t list) in
  (* test if pos is far enough for the sides of the map (10% margin) *)
  let test_dist_edge pos =
    let (a,b) = topair(pos) in
    a > 10*width/100
    && b > 10*height/100
    && a < 90*width/100
    && b < 90*height/100
  in
  (* find possibles armies central points, the spawns, represented by their general.
    A position is considered valid iff it is the center of a 3*3 plain square *)
  Battlefield.tile_iteri
    (
      let is_plain t = Tile.get_name t = "plain" in
      fun pos ti ->
        let nei = neighbors_corners m pos in
        if test_dist_edge pos
            && Tile.get_name ti = "plain"
            && count is_plain nei = 8 then
          poslist := pos:: !poslist
    )
    m;
  if List.length !poslist < nbplayers then
    raise NotEnoughSpawns
  else
    !poslist

(* positions nbplayers armies on a map m, with legit_spawns the list returned by init_positioning *)
let positioning m playerslist legit_spawns neutral_buildings =
  let nbplayers = List.length playerslist in
  let (width,height) = Battlefield.size m in
  let rec behead = function
  | 0,_ -> []
  | n,[] -> raise BadSpawnsSequence
  | n,p::q -> p::(behead (n-1,q))
  in
  let sqrt_expansion = int_of_float (sqrt (float_of_int Config.config#settings_engine.island_expansion)) in
  (* test if a given position is far enough for all the previously selected positions.
    Far enough means that the distance between the two positions is at least 90% of the map size, on the number of player. *)
  let rec test_dist_spawns p = function
    | [] -> true
    | p1::q ->
        (Position.dist p p1 > ((width + height)*sqrt_expansion)/(10*(max 1 nbplayers)))
        && test_dist_spawns p q
  in
  (* extract a random valid sequence of spawns *)
  let filtered_pos = ref [] in
  List.iter
    (
      fun pos ->
        if test_dist_spawns pos !filtered_pos && not (List.exists (fun b -> b#position = pos) neutral_buildings) then
          filtered_pos := pos :: !filtered_pos
    )
    (Utils.shuffle legit_spawns);
  (* check if there are enough of them (more than nbplayers) *)
  let poslist = behead (nbplayers, Utils.shuffle !filtered_pos) in
  (* check the existence of a path between all armies *)
  check_path (m,(),poslist);

  (* place an army around the position spawn, knowing the other armies positions (to avoid overlaps on small maps)*)
  let position_army_around spawn (player:Player.logicPlayer) other_armies_pos =
    let unbound_units_list = Config.config#unbound_units_list in
    let unbound_buildings_list = Config.config#unbound_buildings_list in
    let base = Building.bind (Config.config#unbound_building "base") spawn (Some player#get_id) in
    player#add_building base;
    player#set_base base;
    let army = ref [] in
    let buildings = ref [base] in
    let army_pos = ref [spawn] in
    List.iter
      (
        fun ui ->
          for i = 0 to ui#spawn_number - 1 do
            let ne = List.filter
              (
                fun p ->
                  Battlefield.in_range m p
                  && (Tile.traversable_m (Battlefield.get_tile m p) ui#movement_type)
                  && not (List.mem p other_armies_pos)
              )
              (neighbours !army_pos) in
            if ne = [] then
              raise NotEnoughPlace
            else
              (
                let r = Random.int (List.length ne) in
                let pos = List.nth ne r in
                let binded_ui = Unit.bind ui pos player#get_id in
                player#add_unit binded_ui;
                army := binded_ui :: !army;
                army_pos := pos :: !army_pos;
              )
          done;
      )
      unbound_units_list;
    List.iter
      (
        fun ui ->
          for i = 0 to ui#spawn_number_per_player - 1 do
            let ne = List.filter
              (
                fun p ->
                  Battlefield.in_range m p
                  && (List.for_all (Tile.traversable_m (Battlefield.get_tile m p)) ui#movement_types)
                  && not (List.mem p other_armies_pos)
              )
              (neighbours !army_pos) in
            if ne = [] then
              raise NotEnoughPlace
            else
              (
                let r = Random.int (List.length ne) in
                let pos = List.nth ne r in
                let binded_ui = Building.bind ui pos (Some player#get_id) in
                player#add_building binded_ui;
                buildings := binded_ui :: !buildings;
                army_pos := pos :: !army_pos;
              )
          done;
      )
      unbound_buildings_list;
    (!army, (!army_pos)@other_armies_pos, !buildings)
  in
  (* iter position_army_around for all armies*)
  let rec position_armies n = function
  | [] -> (([]:Unit.t list list),(List.map (fun b -> b#position) neutral_buildings),neutral_buildings)
  | p::q ->
      let (oarmy,ototalpos,obuildings) = position_armies (n+1) q in
      let (army,totalpos,buildings) = position_army_around (List.nth poslist n) p ototalpos in
      (army::oarmy,totalpos,buildings@obuildings)
  in
  let (army,totalpos,buildings) = position_armies 0 playerslist in
  (army, buildings, poslist)


(* create roads and bridges on a map*)
let create_roads m = () (*TODO*)


(* create beaches and other (?) borders of `Block *)
let create_borders m =
  let borders = Config.config#tiles_list in
  (* create the border (water : string, rate : int (0-1000), expansion : int) composed of beach tiles *)
  let create_border (water, rate, expansion) beach =
    let poslist_water =
      Battlefield.tile_filteri
        (fun pos t ->
          Tile.get_name t = water
          && count (fun u -> Tile.get_name u = water) (neighbors_corners m pos) <> 8)
        m
      in
    let poslist_beach =
      Utils.shuffle (List.filter
          (fun pos ->
            let t = Battlefield.get_tile m pos in
            Tile.get_name t <> water
            && match Tile.compare_walkability t beach with | None -> false | Some i -> i = 0)
          (List.filter
            (Battlefield.in_range m)
            (neighbours_corners poslist_water)
          ) )
    in
    (* isolates a proportion of possible beach seeds*)
    let rec behead = function
    | 0,_ -> []
    | n,[] -> assert false
    | n,p::q -> p::(behead (n-1,q))
    in
    let seeds_beach = behead (rate*(List.length poslist_beach)/1000, poslist_beach ) in
    (* grow beaches around those seeds*)
    let grow_border m seed =
      let bord = ref [seed] in
      for i = 1 to expansion do
        bord := !bord @ (List.filter
                          (fun pos ->
                            Battlefield.in_range m pos
                            && List.mem pos poslist_beach)
                          (neighbours_corners !bord));
      done;
      !bord
    in
    List.iter
      (fun pos -> Battlefield.set_tile m pos beach)
      (List.fold_left
        (fun l e -> (grow_border m e) @ l)
        [] seeds_beach
      )
  in
  (* iterates over all `Border tiles *)
  List.iter
    (fun beach ->
      match Tile.get_structure beach with
      | `Border param -> create_border param beach
      | _ -> ()
    )
    borders

let create_buildings m =
  let unbound_list = Config.config#unbound_buildings_list in
  let poslist = ref [] in
  let takenlist = ref [] in
  let rec position ub =function
  | 0 -> []
  | nb -> if !poslist = [] then [] else (
    poslist := Utils.shuffle (!poslist);
    let pos = List.hd !poslist in
    poslist := List.tl !poslist;
    takenlist := pos :: !takenlist;
    let b = Building.bind ub pos None in
    if ub#name = "port" then Battlefield.set_tile m pos (Config.config#tile "port_beach");
    b::(position ub (nb-1)))
  in
  List.fold_left 
    (fun l e -> 
      poslist := Battlefield.tile_filteri 
        (fun pos t -> 
          not (List.mem pos !takenlist)
          && ((e#name <> "port" && (Tile.get_name t <> "beach" && Tile.get_name t <> "lake_beach") && List.for_all (Tile.traversable_m t) e#movement_types) || (e#name = "port" && (Tile.get_name t = "beach"|| Tile.get_name t = "lake_beach")))
        ) m;
      (position e e#spawn_number_neutral)@l
    ) [] unbound_list

(* create structures on a map *)
let create_structs m =
  create_borders m;
  create_roads m;
  create_buildings m

(* iterated tries to spawn armies *)
let units_spawn m playerslist legit_spawns buildings =
  let units_spawn_attempts = Config.config#settings_engine.units_spawn_attempts in
  let rec units_spawn_aux = function
  | 0 -> raise UnitsSpawnFail
  | n ->
    begin
      GenLog.infof "    attempt %d / %d : " (units_spawn_attempts - n +1) units_spawn_attempts;
      try
        let (army,buildings,spawns) = positioning m playerslist legit_spawns buildings in
        let attempt = (m,army,spawns) in
        GenLog.infof "    armies spawned, checking... ";
        flush_all();
        check_movement attempt;
        check_superposed_units attempt;
        (*check_path attempt;*) (* place here any checks on units positioning*)
        (army,buildings)
      with
      | BadSpawnsSequence ->
          GenLog.infof "    Not enough spawns found";
          units_spawn_aux (n-1)
      | NotEnoughPlace ->
          GenLog.infof "    Not enough space around spawn for army";
          units_spawn_aux (n-1)
      | InvalidPositioning ->
          GenLog.infof "    Unit placed on an area not coresponding to its movement modes";
          units_spawn_aux (n-1)
      | UnitsSuperposition ->
          GenLog.infof "    Units superposition";
          units_spawn_aux (n-1)
      | NoPath ->
          GenLog.infof "    No path between armies";
          units_spawn_aux (n-1)
    end
  in
  GenLog.infof "  Spawning armies ...";
  units_spawn_aux units_spawn_attempts

(* iterated tries to create structures *)
let create_structures m =
  let structs_attempts = Config.config#settings_engine.structs_attempts in
  let rec create_structures_aux = function
  | 0 -> raise StructSpawnFail
  | n ->
    begin
      GenLog.infof "    attempt %d / %d : " (structs_attempts - n +1) structs_attempts;
      try
        let buildings = create_structs m in
        GenLog.infof "    structures spawn success";
        buildings
        (* place here any checks on structures positioning*)
      with
      | StructSpawnFail ->
          raise StructSpawnFail
      | NotEnoughPlace ->
          GenLog.infof "    Not enough space for neutral buildings";
          create_structures_aux (n-1)
    end
  in
  GenLog.infof "  Spawning structures ...";
  create_structures_aux structs_attempts

(* iterated tries to generate the map *)
let generate playerslist =
  let generate_attempts = Config.config#settings_engine.generate_attempts in
  let rec generate_aux = function
  | 0 ->
    GenLog.errorf "generator failed, not enough tries? bad calling arguments?";
    raise GeneratorFailure
  | n ->
    begin
      GenLog.infof "  attempt %d / %d : " (generate_attempts - n +1) generate_attempts;
      try
        let m = 
          match Config.config#settings_engine.generation_method with
          | `Dummy -> dummy_gen "plain"
          | `Swap -> swap_gen()
          | `Seeds -> seeds_gen()
          | `Island -> seeds_island()
        in
        let nb = create_structures m in
        let (a,b) = units_spawn m playerslist (init_positioning m (List.length playerslist)) nb in
        let attempt = (m,a,b) in
        GenLog.infof "Generation success"(* place here any check on map generation*);
        attempt
      with
      | StructSpawnFail ->
          GenLog.infof "  structures spawn aborted";
          generate_aux (n-1)
      | NotEnoughSpawns ->
          GenLog.infof "  Spawning armies ...";
          GenLog.infof "    not enough valid spawns";
          GenLog.infof "  armies spawn aborted";
          generate_aux (n-1)
      | UnitsSpawnFail ->
          GenLog.infof "  armies spawn aborted";
          generate_aux (n-1)
    end
  in
  GenLog.infof "Generating Battlefield ...";
  generate_aux generate_attempts


class t (playerslist:Player.logicPlayer list)=
object (self)
  val g = Random.self_init(); generate playerslist
  method field = let m,_,_ = g in m
  method armies = let _,a,_ = g in a
  method buildings = let _,_,b = g in b
  method neutral_buildings = let _,_,b = g in List.filter (fun bu -> bu#player_id = None) b
  method cursor_init_positions = let _,a,b = g in 
    let tbl = Hashtbl.create 10 in 
    match Config.config#settings_engine.cursor_init with
    | `Base ->
        List.iter
          (fun bu ->
            (match bu#player_id with
            | Some a -> Hashtbl.add tbl a bu#position 
            | None -> ())
          )
          (List.filter (fun bu -> bu#name = "base") b);tbl
    | `Unit -> 
        let l = List.fold_left (fun l ua -> ua@l) [] a in
        let list_units = List.filter (fun u -> List.for_all (fun ub -> ub#position <> left u#position) l) l in
        List.iter
          (fun u ->
            Hashtbl.add tbl u#player_id u#position
          )
          list_units;tbl
end
