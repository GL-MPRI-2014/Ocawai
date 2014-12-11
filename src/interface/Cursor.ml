type cursor_state = 
    Idle 
  | Displace of Battlefield.t * Unit.t * Logics.accessibles
  | Action of Unit.t * Position.t * Unit.t list

class cursor ~position = object(self)

  val mutable current_position : Position.t = position
  val mutable state = Idle
  val mutable path = Path.empty
  val mutable scale = 1.
  val mutable offset = (0.,0.)

  initializer
    (* Let's interpolate that ! *)
    ignore (Interpolators.new_sine_ip 
      (fun s -> scale <- s) 3. (1./.20.) 1.)

  method private find_closest_to_dir from_p to_p ul = 
    let (dx, dy) = 
      Position.diff to_p from_p
      |> Position.topair 
    in
    let (dx', dy') = 
      if abs dx >= abs dy && dx >= 0 then (1,0)
      else if abs dx <= abs dy && dy >= 0 then (0,1)
      else if abs dx >= abs dy && dx <= 0 then (-1,0)
      else (0,-1)
    in
    Printf.printf "Dir : %i/%i\n" dx' dy';
    let (fx, fy) = Position.topair from_p in
    Printf.printf "From : %i/%i\n" fx fy;
    let scal_list = List.map (fun u ->
      let (x,y) = Position.topair (Position.diff u#position from_p) in 
      (u, x * dx' + y * dy', abs(x * dy' - y * dx') )
    ) ul in
    Printf.printf "New list : \n";
    let new_list = List.filter (fun (_,b,_) -> b > 0) scal_list in
    List.iter (fun (u,b,c) -> 
      let (ux, uy) = Position.topair u#position in 
      Printf.printf "%i/%i  %i/%i" ux uy b c; print_endline "")
    new_list;
    let sorted_list = List.sort (fun (_,b,c) (_,b',c') -> 
      if b + 2*c < b' + 2*c' then -1 else if b+2*c > b'+2*c' then 1 else 0)
      new_list
    in
    if sorted_list = [] then from_p
    else let (u,_,_) = List.hd sorted_list in u#position

  method set_position pos =
    match state with
    |Displace(map,u,(range,table)) -> 
        if List.mem pos range then begin
          let p' = 
            try Path.reach path pos 
            with Path.Path_exception _ -> Hashtbl.find table pos
          in
          if Path.cost u#movement_type map p' > u#move_range then
            path <- Hashtbl.find table pos
          else path <- p'
        end;
        current_position <- pos ;
        pos
    |Action(_,_,ul) ->
      let pos = self#find_closest_to_dir current_position pos ul in
      current_position <- pos; pos
    | _ -> current_position <- pos; pos

  method position =
    current_position

  method set_state s =
    state <- s;
    match state with 
    |Displace(_) -> path <- Path.init current_position
    |Idle -> path <- Path.empty
    | _ -> ()

  method get_state  = state

  method get_move =
    Path.get_move path

  method scale = scale

  method set_offset o = offset <- o

  method offset = offset
end
