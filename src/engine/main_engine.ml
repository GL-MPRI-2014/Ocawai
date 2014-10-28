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


let () =
begin
  let gen = new FieldGenerator.t 45 45 2 10 5 in
  let dij = Path.dijkstra (gen#field) (List.hd gen#spawns) (Unit.Walk) in
  let r = dij (List.nth gen#spawns 1) in
  print_ascii gen#field gen#armies (match r with | None -> Path.empty | Some (_,b) -> b) gen#spawns;
  print_endline ("path length between spawns 1 and 2 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a));
end
