open Position

exception NotEnoughSpawns
exception NotEnoughPlace
exception InvalidPlacement

(*renvoie la liste des voisins d'une case*)
let neighbors m pos = 
  let (width,height) = Battlefield.size m in 
  let l = ref ([] : Tile.t list) in
  let (x,y) = topair pos in begin
  if x > 0 then begin
    if y > 0 then
      l := (Battlefield.get_tile m (left (up pos)))::( !l);
    l := (Battlefield.get_tile m (left pos))::( !l);
    if y < height-1 then
      l := (Battlefield.get_tile m (left (down pos)))::( !l);
  end;
  if y > 0 then
    l := (Battlefield.get_tile m (up pos))::( !l);
  if y < height-1 then
    l := (Battlefield.get_tile m (down pos))::( !l);
  if x < width -1 then begin
    if y > 0 then
      l := (Battlefield.get_tile m (right (up pos)))::( !l);
    l := (Battlefield.get_tile m (right pos))::( !l);
    if y < height-1 then
      l := (Battlefield.get_tile m (right (down pos)))::( !l);
  end;
  !l
end

let rec count f = function
|p::q when f p -> 1 + (count f q)
|p::q -> count f q
|[] -> 0

    
let swap_gen width height =
  Random.self_init();
  
  let ti_list = Ag_util.Json.from_file Tile_j.read_t_list "resources/config/tiles.json" in
  let tiles = List.map (fun ti -> Tile.tile_t_to_t ti) ti_list in
  let tile a = List.find (fun ti -> Tile.get_name ti = a) tiles in (*liste des tiles*)
  let m = Battlefield.create width height (tile "water") in begin

    let total_density =
      let rec sum = function
      | p::q -> Tile.get_density p + sum q
      | [] -> 0
      in sum tiles
    in
    let rec nth_dens n = function
    | p::q when Tile.get_density p < n -> nth_dens (n - Tile.get_density p) q
    | p::q when Tile.get_density p >= n -> p
    | _ -> failwith("FieldGenerator.dummy_gen : failure nth_dens")
    in
    (*remplir aleatoirement la map, en tenant compte des densitys*)
    for i = 0 to (width - 1) do
      for j = 0 to (height - 1) do
        let r = Random.int total_density +1 in
        Battlefield.set_tile m (create(i,j))  (nth_dens r tiles);
      done;
    done;
    (* degre de contiguite d'une position = nb de voisins identiques / nb de voisins*)
    let contiguite pos = 
      let tpos = Battlefield.get_tile m pos in
      let nei = neighbors m pos in
      (float_of_int (count (fun t -> Tile.get_name t = Tile.get_name tpos) nei) /. float_of_int (List.length nei))
    in
    
    let swap pos1 pos2 = 
      let t1 = Battlefield.get_tile m pos1 in begin
        Battlefield.set_tile m pos1 (Battlefield.get_tile m pos2);
        Battlefield.set_tile m pos2 t1
      end in
    (* on prends 2 positions random, on les swap si ca augmente la contiguite, et on itere*)
    for i = 0 to 50 * width * height (* arbitraire *) do
      let pos1 = create (Random.int width , Random.int height) in
      let pos2 = create (Random.int width , Random.int height) in
      let previous = contiguite pos1 +. contiguite pos2 in
      swap pos1 pos2;
      if previous > contiguite pos1 +. contiguite pos2 then
        swap pos1 pos2;
    done;
    m
  end
  
let placement m nbplayers =
  let (width,height) = Battlefield.size m in 
  let poslist = ref ([]:Position.t list) in
  let dist pos1 pos2 = let (x1,y1) = topair pos1 in let (x2,y2) = topair pos2 in (abs (x2-x1)) + (abs (y2-y1)) in
  let init_positions () = begin
    for i = 0 to (width - 1) do
      for j = 0 to (height - 1) do
        let pos = create(i,j) in
        let nei = neighbors m pos in
        let name = Tile.get_name (Battlefield.get_tile m (create(i,j))) in
        if name = "plain" then
          if count (fun t -> Tile.get_name t = "plain") nei = 8 then
            poslist := pos::( !poslist);
      done;
    done;
    (*print_endline ("poslist size "^string_of_int (List.length ( !poslist)));*)
    let rec condition p = function
      | [] -> true
      | p1::q -> (dist p p1 > (width + height)/nbplayers) && condition p q 
    in
    let filtered_pos = ref [] in 
    begin
      for i = 0 to List.length ( !poslist) do
        let r = Random.int (List.length ( !poslist)) in
        let pos = List.nth ( !poslist) r in
        if condition pos ( !filtered_pos) then filtered_pos := pos :: ( !filtered_pos);
      done;
      print_endline (string_of_int (List.length ( !filtered_pos))^" possibles spawns");
      if List.length ( !filtered_pos) < nbplayers then raise NotEnoughSpawns;
      poslist := [];
      let rec add_elt elt = function
        |[] -> [elt]
        |t::q when t = elt -> t::q 
        |t::q when t > elt -> elt::t::q
        |t::q -> t::(add_elt elt q)
      in 
      while List.length ( !poslist) < nbplayers do (* put nbplayers positions in poslist*)
        let r = Random.int (List.length ( !filtered_pos)) in
        poslist := add_elt (List.nth ( !filtered_pos) r) ( !poslist);
      done;
    end
  end in
  
  let place_army_around spawn =
  let ui_list = Ag_util.Json.from_file Unit_j.read_t_list "resources/config/units.json" in
  let army = ref [] in 
  let army_pos = ref [spawn] in
  begin
  List.iter (fun ui -> 
    for i = 1 to ui.Unit_t.spawn_number do
      let ne = List.filter (fun p -> (not (out_of_bounds p (create(0,0)) (up (left (create (width,height)))))) && let name = Tile.get_name (Battlefield.get_tile m p) in ( name <> "water" && name <> "mountain" ) ) (neighbours ( !army_pos)) in
      if ne = [] then raise NotEnoughPlace else
      let r = Random.int (List.length ne) in
      let pos = List.nth ne r in
      begin
        army := (Unit.create_from_unit_t ui pos) :: ( !army);
        army_pos := pos :: (!army_pos);
      end
    done;
  ) ui_list;
  !army
  end in
  
  let placement_army n =
  let spawn = List.nth ( !poslist) (n-1) in
    place_army_around spawn 
  (*List.map (fun pos -> Unit.create_from_config "infantry" pos) ( !poslist) (* temporary, will be changed when interface will render several players*) *)
  in
  let rec placement_armies = function
  | 0 -> ([[]]:Unit.t list list) (* a remplacer par ([]:Unit.t list list) pour separer les armees *)
  | n when n > 0 -> let others = placement_armies (n-1) in
                    [(placement_army n)@(List.hd others)] (* fusionne toutes les armes au player 1, a remplacer par (placement_army n)::others *)
  | _ -> failwith("generate : nbplayer < 0")
  in (init_positions(); placement_armies nbplayers)

(* un placement est valide si les unites ne sont pas placees sur de l'eau ou des montagnes *)
let test attempt = let (m,a) = attempt in
  if not (List.for_all (List.for_all (fun u -> let name = Tile.get_name (Battlefield.get_tile m (u#position)) in name <> "water" && name <> "mountain")) a)
    then raise InvalidPlacement else ()

let generate width height nbplayers nbattempts=
  let rec generate_aux = function
  | 0 -> failwith("generator failed, try more attempts")
  | n -> 
    begin
      print_endline ("attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^" ..."); 
      try
        begin
          let attempt =
            let m = swap_gen width height in
              ( m , placement m nbplayers )
          in (test attempt; attempt) 
        end
      with
      | NotEnoughSpawns -> (print_endline " Not enough spawns"; generate_aux (n-1) )
      | InvalidPlacement -> (print_endline " Unit placed on water/mountain"; generate_aux (n-1) )
      | NotEnoughPlace -> (print_endline " Not enough space around spawn for army"; generate_aux (n-1) )
    end
  in generate_aux nbattempts

class t (width:int) (height:int) (nbplayers:int) = 
object (self)
  val g = (print_string "Generating valid Battlefield, ";generate width height nbplayers 20)
  method field = fst g
  method armies = snd g
end

