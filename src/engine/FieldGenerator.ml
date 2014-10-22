
let generate width height =
  let tiles = [("water", Tile.create_from_file "water" "");
    ("forest", Tile.create_from_file "forest" "");
    ("plain", Tile.create_from_file "plain" "");
    ("concrete", Tile.create_from_file "concrete" "")] in
  let map = Battlefield.create width height (List.assoc "water" tiles) in
  map
