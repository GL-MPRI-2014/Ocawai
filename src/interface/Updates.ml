open Types

type turn =
  | Your_turn
  | Turn_of of int
  | Nobody_s_turn

type animation =
  | Moving_unit of Unit.t * Position.t list
  | Attack
  | Pause of int
  | Nothing

(* Number of frames for a unit to run through a tile *)
let walking_time = 3

(* Number of frames of the focus on an attacked unit *)
let attack_time = 30

class handler data camera = object(self)

  (* Holds the animation ongoing *)
  val mutable current_animation = Nothing

  (* Number of frames since the beginning of the animation *)
  val mutable frame_counter = 0

  (* Last staged update *)
  val mutable last_update = None

  (* Current player *)
  val mutable current_turn = Nobody_s_turn

  (* Tells if a position is foggy *)
  method private foggy p =
    let (i,j) = Position.topair p in
    try data#actual_player#get_fog.(i).(j) = 0
    with _ -> false

  method private visible p =
    not (self#foggy p)

  (* The purpose of this method is to do the update for real *)
  method private ack_update : Types.update -> unit = function
    | Game_over -> () (* TODO *)
    | Your_turn ->
        current_turn <- Your_turn
    | Turn_of id ->
        current_turn <- Turn_of id
    | Classement -> () (* WTF?! TODO ? *)
    | Set_army (army,pid) ->
        (* TODO Check if we're not doing it for nothing *)
        let player = Logics.find_player pid data#players in
        List.map Oo.copy army
        |> player#set_army
    | Set_building (buildings,pid) ->
        (* TODO Same here *)
        let player = Logics.find_player pid data#players in
        List.map Oo.copy buildings
        |> player#set_buildings
    | Add_unit (u,pid) ->
        let player = Logics.find_player pid data#players in
        Oo.copy u
        |> player#add_unit
    | Add_building (b,pid) ->
        let player = Logics.find_player pid data#players in
        (* assert (b#player_id = Some (player#get_id)) ; *)
        Oo.copy b
        |> player#add_building
    | Delete_unit (uid,pid) ->
        let player = Logics.find_player pid data#players in
        player#delete_unit uid
    | Delete_building (bid,pid) ->
        let player = Logics.find_player pid data#players in
        (List.find
          (fun b -> b#get_id = bid)
          data#neutral_buildings)#set_neutral ;
        player#delete_building bid
    | Move_unit (uid,path,pid) ->
        let player = Logics.find_player pid data#players in
        player#move_unit uid path
    | Set_unit_hp (uid,hp,pid) ->
        let player = Logics.find_player pid data#players in
        (player#get_unit_by_id uid)#set_hp hp
    | Set_unit_played (uid,pid,b) ->
        let player = Logics.find_player pid data#players in
        (player#get_unit_by_id uid)#set_played b
    | Harvest_income ->
        (data#actual_player :> Player.logicPlayer)#harvest_buildings_income
    | Use_resource price ->
        (data#actual_player :> Player.logicPlayer)#use_resource price
    | Set_client_player _ -> ()
    | Set_logic_player_list _ -> ()
    | Map _ -> ()
    | Building_changed b ->
        let b = Oo.copy b in
        data#toggle_neutral_building b


  method private ack_staged =
    begin
      match last_update with
      | Some u -> self#ack_update u
      | None -> ()
    end ;
    last_update <- None

  method private stage_ack u =
    match last_update with
    | None    -> last_update <- Some u
    | Some u' -> self#ack_staged ; last_update <- Some u

  method private read_update =
    (* We reset since it will be a new animation either way *)
    frame_counter <- 0 ;
    (* We ack the last update staged if necessary *)
    self#ack_staged ;
    (* Reading oldest unread update *)
    begin match data#pop_update with
    | Some u ->
        begin match u with
          | Move_unit (un,path,id_p) ->
              (* We only take it into account if there is a visible part *)
              if List.exists self#visible path then begin
                let vpath = List.filter self#visible path in
                camera#set_position (List.nth vpath (List.length vpath - 1)) ;
                Sounds.play_sound "boots" ;
                let player = Logics.find_player id_p data#players in
                current_animation <-
                  Moving_unit (player#get_unit_by_id un, path) ;
                self#ack_update u
              end
              else (self#ack_update u ; self#read_update)
          | Game_over ->
              Sounds.play_sound "lose" ;
              (* TODO Animation *)
              self#ack_update u ;
              (* There should'nt be any update but still... *)
              self#read_update
          | Set_unit_hp (uid,_,pid) ->
              self#stage_ack u ;
              let player = Logics.find_player pid data#players in
              let u = player#get_unit_by_id uid in
              if self#visible u#position then begin
                camera#set_position u#position ;
                camera#cursor#set_state Cursor.Watched_attack ;
                Sounds.play_sound "shots" ;
                current_animation <- Attack
                (* TODO Add animations *)
              end
              else self#read_update
          | Building_changed b ->
              if self#visible b#position then
                camera#set_position b#position ;
              (* TODO Play some sound here? *)
              (* data#toggle_neutral_building b ; *)
              self#ack_update u ;
              (* TODO Add some animation? *)
              self#read_update
          | Your_turn ->
              (* TODO Animation. One needed for the others turn. *)
              (* TODO Center the camera on the player (how?) *)
              (* camera#set_position (data#actual_player) *)
              self#ack_update u ;
              self#read_update
          | _ ->
              (* TODO Stop ignoring them *)
              self#ack_update u ;
              self#read_update
        end
    | None -> ()
    end

  method private process_animation =
    match current_animation with
    | Moving_unit (u,path) ->
        if frame_counter + 1 = walking_time then
        begin
          frame_counter <- 0 ;
          match path with
          | [] -> current_animation <- Pause 20
          | e :: r -> current_animation <- Moving_unit (u, r)
        end
        else frame_counter <- frame_counter + 1
    | Attack ->
        if frame_counter + 1 = attack_time
        then begin
          current_animation <- Nothing ;
          camera#cursor#set_state Cursor.Idle
        end
        else frame_counter <- frame_counter + 1
    | Pause 0 -> current_animation <- Nothing
    | Pause i -> current_animation <- Pause (i-1)
    | Nothing -> 
        Mutex.unlock data#mutex; 
        Thread.yield ()

  method update =
    Mutex.unlock data#mutex;
    Mutex.lock data#mutex;
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

  method current_turn = current_turn

end
