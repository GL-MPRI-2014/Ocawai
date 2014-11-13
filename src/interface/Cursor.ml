type cursor_state = 
    Idle 
  | Displace of Battlefield.t * Unit.t * Logics.accessibles
  | Action of Unit.t * Position.t

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
        true
    |Action(u, p) -> 
        let range = Position.range p 1 u#attack_range in
        if List.mem pos range then begin
          current_position <- pos;
          true
        end else false
    | _ -> current_position <- pos; true

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
