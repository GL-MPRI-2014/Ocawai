open Position

let dummy_gen width height =
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
    (*renvoie la liste des voisins d'une case*)
    let neighbors pos = let l = ref ([] : Tile.t list) in let (x,y) = topair pos in begin
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
    end in
    (* degre de contiguite d'une position = nb de voisins identiques / nb de voisins*)
    let contiguite pos = 
      let tpos = Battlefield.get_tile m pos in
      let nei = neighbors pos in
      let rec count f = function
      |p::q when f p -> 1 + (count f q)
      |p::q -> count f q
      |[] -> 0
      in (float_of_int (count (fun t -> Tile.get_name t = Tile.get_name tpos) nei) /. float_of_int (List.length nei))
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
  
(* un placement est valide si les unites ne sont pas placees sur de l'eau ou des montagnes *)
let test attempt = let (m,a) = attempt in
List.for_all (List.for_all (fun u -> let name = Tile.get_name (Battlefield.get_tile m (u#position)) in name <> "water" && name <> "mountain")) a

let placement_armies m nbplayers = 
  let rec empt = function
  | 0 -> ([]:Unit.t list list)
  | n when n > 0 -> ([]:Unit.t list)::(empt (n-1))
  | _ -> failwith("generate : nbplayer < 0")
  in [
      Unit.create_from_file "infantry" "" (create(41,42));
      Unit.create_from_file "infantry" "" (create(41,39));
      Unit.create_from_file "infantry" "" (create(39,39))
      ]::(empt (nbplayers-1))

let generate width height nbplayers nbattempts=
  let rec generate_aux = function
  | 0 -> failwith("generator failed, try more attempts")
  | n -> (print_endline ("attempt "^(string_of_int (nbattempts - n +1))^" / "^(string_of_int nbattempts)^" ..."); 
    let attempt =
      let m = dummy_gen width height in
        (m,placement_armies m nbplayers)
    in if test attempt then attempt else generate_aux (n-1) )
  in generate_aux nbattempts

class t (width:int) (height:int) (nbplayers:int) = 
object (self)
  val g = (flush_all();print_string "Generating valid Battlefield, ";generate width height nbplayers 50)
  method field = fst g
  method armies = snd g
end

