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

(* generate a map randomly then uses swap_smoothing *)
let swap_gen config =
  (* special tiles used by the generation *)
  let is_seed m p = Tile.get_name (Battlefield.get_tile m p) = "seed" in
  let width = config#settings.map_width in
  let height = config#settings.map_height in
  let tiles_all = config#tiles_list in
  let tiles =
    List.filter
    (fun a -> Tile.get_structure a = `Block)
    tiles_all in
  if (List.length tiles = 0) then failwith("no `Block tiles in config") else
  let total = total_density tiles in
  let m = Battlefield.create width height (List.hd tiles) in

  (* fill the map with regards to densities *)
  Battlefield.tile_iteri
    (fun p _ -> Battlefield.set_tile m p (get_tile_with_density total tiles))
    m;
  swap_smoothing m 50 is_seed;
  (* change tiles with less than two identical neighbors *)
  hard_smoothing m tiles (2./.8.) is_seed;
  m

(* place seeds (generation centers) and expand them until the whole map is filled *)
let seeds_gen config =
  (* special tiles used by the generation *)
  let blank_tile = config#tile "blank" in
  let is_blank m p = Tile.get_name (Battlefield.get_tile m p) = "blank" in
  let is_seed m p = Tile.get_name (Battlefield.get_tile m p) = "seed" in
  let width = config#settings.map_width in
  let height = config#settings.map_height in
  let nbseeds = width*height/50 in
  let dist_min_btw_seeds = 2 in
  let tiles_all = config#tiles_list in
  let tiles =
    List.filter
    (fun a -> Tile.get_structure a = `Block)
    tiles_all in
  if (List.length tiles = 0) then failwith("no `Block tiles in config") else
  let m = Battlefield.create width height blank_tile in
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
                                            (config#tile "seed") instead of t on the following statement*)
             )
        )
        (p::(eucl_disk m p dist_min_btw_seeds))
    then
    (
      Battlefield.set_tile m p t; (* option : replace t by (config#tile "seed") in order to visualize the seeds on the map
                                     note : the previous test is affected, use with caution *)
      (t,p)::(create_seeds (n-1))
    )
    else
      create_seeds (n-1)
  in
  (* grows seed t on a position list *)
  let rec grow_list m t = function
  | [] -> ()
  | p::q ->
    if is_blank m p then
      Battlefield.set_tile m p t;
    grow_list m t q
  in
  (* extract at most n elements of a list*)
  let rec behead = function
  | 0,_ -> []
  | n,_ when n<0 -> assert false
  | n,[] -> []
  | n,p::q -> p::(behead (n-1,q))
  in
  (* grow seed t on nb_grow tiles of its neighbors position set setpos*)
  let grow_seed t nb_grow setpos =
    let sl = behead (nb_grow,Utils.shuffle (SetPos.fold (fun e l -> e::l) setpos [])) in
    grow_list m t sl;
    SetPos.filter
      (fun p -> is_blank m p)
      (SetPos.union
        (SetPos.filter
          (fun p -> Battlefield.in_range m p)
          (neighbours_set sl)
        )
        setpos
      )
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
                t, grow_seed t
                  (max
                    1
                    (Tile.get_grow_speed t)
                  ) s
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
  m


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
let positioning m playerslist legit_spawns config =
  let nbplayers = List.length playerslist in
  let (width,height) = Battlefield.size m in
  let rec behead = function
  | 0,_ -> []
  | n,[] -> raise BadSpawnsSequence
  | n,p::q -> p::(behead (n-1,q))
  in
  (* test if a given position is far enough for all the previously selected positions.
    Far enough means that the distance between the two positions is at least 90% of the map size, on the number of player. *)
  let rec test_dist_spawns p = function
    | [] -> true
    | p1::q ->
        (Position.dist p p1 > (90*width + 90*height)/(100*nbplayers))
        && test_dist_spawns p q
  in
  (* extract a random valid sequence of spawns *)
  let filtered_pos = ref [] in
  List.iter
    (
      fun pos ->
        if test_dist_spawns pos !filtered_pos then
          filtered_pos := pos :: !filtered_pos
    )
    (Utils.shuffle legit_spawns);
  (* check if there are enough of them (more than nbplayers) *)
  let poslist = behead (nbplayers, Utils.shuffle !filtered_pos) in
  (* check the existence of a path between all armies *)
  check_path (m,(),poslist);

  (* place an army around the position spawn, knowing the other armies positions (to avoid overlaps on small maps)*)
  let position_army_around spawn player other_armies_pos config =
    let unbound_list = config#unbound_units_list in
    let general = Unit.bind (config#unbound_unit "general") spawn player#get_id in
    player#add_unit general;
    let army = ref [general] in
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
      unbound_list;
    (!army, (!army_pos)@other_armies_pos)
  in
  (* iter position_army_around for all armies*)
  let rec position_armies n = function
  | [] -> (([]:Unit.t list list),([]:Position.t list))
  | p::q ->
      let others = position_armies (n+1) q in
      let ap = position_army_around (List.nth poslist n) p (snd others) config in
      ((fst ap)::(fst others),snd ap)
  in
  (fst (position_armies 0 playerslist), poslist)


(* create roads and bridges on a map*)
let create_roads m config = () (*TODO*)


(* create beaches and other (?) borders of `Block *)
let create_borders m config =
  let borders = config#tiles_list in
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

(* create structures on a map *)
let create_structs m config =
  create_borders m config;
  create_roads m config

(* iterated tries to spawn armies *)
let units_spawn m playerslist legit_spawns config =
  let units_spawn_attempts = config#settings_engine.units_spawn_attempts in
  let rec units_spawn_aux = function
  | 0 -> raise UnitsSpawnFail
  | n ->
    begin
      print_string ("    attempt "^(string_of_int (units_spawn_attempts - n +1))^" / "^(string_of_int units_spawn_attempts)^": ");
      try
        let (a,sp) = positioning m playerslist legit_spawns config in
        let attempt = (m,a,sp) in
        print_string "armies spawned, checking... ";
        flush_all();
        check_movement attempt;
        check_superposed_units attempt;
        (*check_path attempt;*) (* place here any checks on units positioning*)
        print_endline "success";
        (a,sp)
      with
      | BadSpawnsSequence ->
          print_endline "Not enough spawns found";
          units_spawn_aux (n-1)
      | NotEnoughPlace ->
          print_endline " Not enough space around spawn for army";
          units_spawn_aux (n-1)
      | InvalidPositioning ->
          print_endline "Unit placed on an area not coresponding to its movement modes";
          units_spawn_aux (n-1)
      | UnitsSuperposition ->
          print_endline "Units superposition";
          units_spawn_aux (n-1)
      | NoPath ->
          print_endline "No path between armies";
          units_spawn_aux (n-1)
    end
  in
  print_endline "  Spawning armies ...";
  units_spawn_aux units_spawn_attempts

(* iterated tries to create structures *)
let create_structures m config=
  let structs_attempts = config#settings_engine.structs_attempts in
  let rec create_structures_aux = function
  | 0 -> raise StructSpawnFail
  | n ->
    begin
      print_string ("    attempt "^(string_of_int (structs_attempts - n +1))^" / "^(string_of_int structs_attempts)^": ");
      try
        create_structs m config;
        print_endline "structures spawn success"
        (* place here any checks on structures positioning*)
      with
      | StructSpawnFail ->
          print_newline();
          raise StructSpawnFail
    end
  in
  print_endline "  Spawning structures ...";
  create_structures_aux structs_attempts

(* iterated tries to generate the map *)
let generate playerslist config =
  let generate_attempts = config#settings_engine.generate_attempts in
  let rec generate_aux = function
  | 0 ->
    print_endline("generator failed, not enough tries? bad calling arguments?");
    raise GeneratorFailure
  | n ->
    begin
      print_endline ("  attempt "^(string_of_int (generate_attempts - n +1))^" / "^(string_of_int generate_attempts)^": ");
      try
        let m = seeds_gen config in
        create_structures m config;
        let (a,sp) = units_spawn m playerslist (init_positioning m (List.length playerslist)) config in
        let attempt = (m,a,sp) in
        print_endline "Generation success"(* place here any check on map generation*);
        attempt
      with
      | StructSpawnFail ->
          print_endline "  structures spawn aborted";
          generate_aux (n-1)
      | NotEnoughSpawns ->
          print_endline "  Spawning armies ...\n   not enough valid spawns\n  armies spawn aborted";
          generate_aux (n-1)
      | UnitsSpawnFail ->
          print_endline "  armies spawn aborted";
          generate_aux (n-1)
    end
  in
  print_endline "Generating Battlefield : ";
  generate_aux generate_attempts


class t (playerslist:Player.logicPlayer list)=
object (self)
  val g = Random.self_init(); generate playerslist Config.config
  method field = let m,_,_ = g in m
  method armies = let _,a,_ = g in a
  method spawns = let _,_,sp = g in sp
end
