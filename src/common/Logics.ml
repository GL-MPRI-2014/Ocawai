let unit_vision (unit : Unit.t) (bf : Battlefield.t) : Position.t list =
  let l = Position.filled_circle (unit#position) (unit#vision_range) in
  let pmin = Position.create (0,0) in
  let pmax = Position.create (Battlefield.size bf) in
  List.filter (fun pos -> not (Position.out_of_bounds pos pmin pmax)) l


let rec remove_double l = match l with
  | [] -> []
  | h::t -> h :: (remove_double (List.filter (fun e -> e <> h) t))


let player_vision (player : Player.t) (bf : Battlefield.t) : Position.t list =
  let l = List.fold_left 
    (fun list unit -> list @ (unit_vision unit bf))
    [] player#get_army
  in
  remove_double l
  


