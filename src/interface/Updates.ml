open Types

type animation =
  | Moving_unit of Unit.t * Position.t list
  | Nothing

(* Number of frames for a unit to run through a tile *)
let walking_time = 2

class handler data = object(self)

  (* Holds the animation ongoing *)
  val mutable current_animation = Nothing

  (* Number of frames since the beginning of the animation *)
  val mutable frame_counter = 0

  method private read_update =
    (* We reset since it will be a new animation either way *)
    frame_counter <- 0 ;
    (* Reading oldest unread update *)
    match data#pop_update with
    | Some u ->
        begin match u with
          | Move_unit (u,path,id_p) ->
              (* TODO Only take into account visible units *)
              Sounds.play_sound "boots" ;
              let player = Logics.find_player id_p data#players in
              current_animation <- Moving_unit (player#get_unit_by_id u, path)
          | Game_over ->
              Sounds.play_sound "lose" ;
              (* There should'nt be any update but still... *)
              self#read_update
          | Set_unit_hp _ ->
              Sounds.play_sound "shots" ;
              (* TODO Add animations instead of continuing *)
              self#read_update
          | Building_changed b ->
              (* TODO Play some sound here? *)
              data#toggle_neutral_building b ;
              (* TODO Add some animation? *)
              self#read_update
          | _ ->
              (* TODO Stop ignoring them *)
              self#read_update
        end
    | None -> ()

  method private process_animation =
    match current_animation with
    | Moving_unit (u,path) ->
        if frame_counter + 1 = walking_time then
        begin
          frame_counter <- 0 ;
          match path with
          | [] -> current_animation <- Nothing
          | e :: r -> current_animation <- Moving_unit (u, r)
        end
        else frame_counter <- frame_counter + 1
    | Nothing -> ()

  method update =
    if current_animation = Nothing then self#read_update ;
    self#process_animation

  method unit_position u =
    match current_animation with
    | Moving_unit (soldier, e :: n :: _) when soldier = u ->
        (* Beware of magic numbers *)
        let o =
          (float_of_int frame_counter) /. (float_of_int walking_time) *. 50.
        in
        e, Position.(
          if      n = left  e then (-. o,   0.)
          else if n = right e then (o   ,   0.)
          else if n = up    e then (0.  , -. o)
          else if n = down  e then (0.  ,    o)
          else assert false
        )
    | Moving_unit (soldier, e :: _) when soldier = u -> e, (0.,0.)
    | _ -> u#position, (0.,0.)

end
