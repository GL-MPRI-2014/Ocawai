(** main function for testing the engine (only the generation for now) *)

let () = begin
  print_string "\n";
  let gen = new FieldGenerator.t 50 50 2 in
  Battlefield.print gen#field
end
