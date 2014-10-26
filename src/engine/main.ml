(** main function for testing the engine (only the generation for now) *)

let () = begin
  let gen = new FieldGenerator.t 50 50 1 in
  begin
    Battlefield.print gen#field;
    let dij = Pathfinder.dijkstra (gen#field) (Position.create(25,25)) (Unit.Walk) in
    let r = dij (Position.create(0,0)) in print_endline ("25 25 to 0 0 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a));
    let r = dij (Position.create(0,49)) in print_endline ("25 25 to 0 49 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a));
    let r = dij (Position.create(49,0)) in print_endline ("25 25 to 49 0 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a));
    let r = dij (Position.create(49,49)) in print_endline ("25 25 to 49 49 : "^(match r with | None -> "no path" | Some (a,_) -> string_of_int a));
    print_newline();
  end
end
