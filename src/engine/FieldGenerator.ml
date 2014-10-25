open Position

let generate width height =
  Random.self_init();
  let ti_list = Ag_util.Json.from_file Tile_j.read_t_list "resources/config/tiles.json" in
  let tiles_raw = List.map (fun ti -> Tile.tile_t_to_t ti) ti_list in
  let tiles = List.filter (fun ti -> let name = Tile.get_name ti in name <> "mountain" && name <> "concrete") tiles_raw in
  let tile a = List.find (fun ti -> Tile.get_name ti = a) tiles in
  let m = Battlefield.create width height (tile "water") in begin
  print_string "Generating Battlefield...\n";
  for i = 0 to (width - 1) do
    for j = 0 to (height - 1) do
     Battlefield.set_tile m (create(i,j))  (List.nth tiles (Random.int (List.length tiles)));
    done;
  done;
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
  let contiguite pos = 
  let tpos = Battlefield.get_tile m pos in
  let nei = neighbors pos in
  let rec count f = function
  |p::q when f p -> 1 + (count f q)
  |p::q -> count f q
  |[] -> 0
  in (float_of_int (count (fun t -> Tile.get_name t = Tile.get_name tpos) nei) /. float_of_int (List.length nei))
  in
  let swap pos1 pos2 = let t1 = Battlefield.get_tile m pos1 in begin
  Battlefield.set_tile m pos1 (Battlefield.get_tile m pos2);
  Battlefield.set_tile m pos2 t1
  end in
  for i = 0 to 1000000 do
    let pos1 = create (Random.int width , Random.int height) in
    let pos2 = create (Random.int width , Random.int height) in
    let previous = contiguite pos1 +. contiguite pos2 in
    swap pos1 pos2;
    if previous > contiguite pos1 +. contiguite pos2 then
      swap pos1 pos2;
  done;
  m
  end
