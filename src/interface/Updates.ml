open Types

type turn =
  | Your_turn
  | Turn_of of int
  | Nobody_s_turn

type animation =
  | Moving_unit of Unit.t * Position.t list
  | Attack
  | Boom of Position.t
  | Pause of int
  | Nothing

type speed =
  | Slow
  | Normal
  | Fast
  | FFast

(* Logger *)
module Log = Log.Make (struct let section = "Updates" end)
open Log

(* Number of frames for a unit to run through a tile *)
let walking_time = 3

(* Number of frames of the focus on an attacked unit *)
let attack_time = 30

(* Number of frames for a boom *)
let boom_time = 10

class handler data camera = object(self)

  (* Holds the animation ongoing *)
  val mutable current_animation = Nothing

  (* Number of frames since the beginning of the animation *)
  val mutable frame_counter = 0

  (* Half frame counter (for low speed) *)
  val mutable half_frame_counter = 0

  (* Speed of animations *)
  val mutable speed = Normal

  (* Last staged update *)
  val mutable last_update = None

  (* Current player *)
  val mutable current_turn = Nobody_s_turn

  (* Increases the frame counter *)
  method private frame_incr =
    match speed with
    | Slow ->
        if half_frame_counter = 1 then begin
          frame_counter <- frame_counter + 1 ;
          half_frame_counter <- 0
        end
        else half_frame_counter <- 1
    | Normal -> frame_counter <- frame_counter + 1
    | Fast   -> frame_counter <- frame_counter + 2
    | FFast  -> frame_counter <- frame_counter + 4

  (* Log an update *)
  method private log_update = function
    | Game_over -> infof "Game Over"
    | You_win -> infof "You win"
    | Your_turn -> infof "Your turn"
    | Turn_of id -> infof "Turn of P%d" id
    | Classement -> infof "Classement... WTF!?"
    | Set_army _ -> infof "Set army..."
    | Set_building _ -> infof "Set building..."
    | Add_unit (u,pid) -> infof "P%d adds U%d" pid u#get_id
    | Add_building (b,pid) -> infof "P%d adds B%d" pid b#get_id
    | Delete_unit (uid,pid) -> infof "P%d lost U%d" pid uid
    | Delete_building (bid,pid) -> infof "P%d lost B%d" pid bid
    | Move_unit (uid,_,pid) -> infof "U%d of P%d moved" uid pid
    | Set_unit_hp (uid,hp,pid) ->
        infof "P%d's U%d set hp to %d" pid uid hp
    | Set_unit_played (uid,pid,b) ->
        infof "P%d's U%d set played to %B" pid uid b
    | Harvest_income -> infof "Time to harvest income"
    | Use_resource p -> infof "Spent %d flowers" p
    | Set_client_player _ -> infof "Set client player..."
    | Set_logic_player_list _ -> infof "Set logic player list..."
    | Map _ -> infof "Map..."
    | Building_changed b -> infof "Building changed B%d" b#get_id

  (* Tells if a position is foggy *)
  method private foggy p =
    Fog.hidden data#actual_player#get_fog p

  method private visible p =
    not (self#foggy p)

  (* The purpose of this method is to do the update for real *)
  method private ack_update u =
    self#log_update u ;
    match u with
    | Game_over -> () (* TODO *)
    | Your_turn ->
        current_turn <- Your_turn
    | You_win -> () (* TODO *)
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
          | You_win ->
              Sounds.play_sound "yeah" ;
              (* TODO Animation *)
              self#ack_update u ;
              self#read_update
          | Delete_unit (uid,pid) ->
              let player = Logics.find_player pid data#players in
              let un = player#get_unit_by_id uid in
              current_animation <- Boom un#position ;
              self#ack_update u
          | Set_unit_hp (uid,_,pid) ->
              self#stage_ack u ;
              let player = Logics.find_player pid data#players in
              let u = player#get_unit_by_id uid in
              if self#visible u#position then begin
                camera#set_position u#position ;
                camera#cursor#set_state Cursor.Watched_attack ;
                Sounds.play_sound "shots" ;
                current_animation <- Attack
              end
              else self#read_update
          | Building_changed b ->
              if self#visible b#position then
                camera#set_position b#position ;
              (* TODO Play some sound here? *)
              self#ack_update u ;
              (* TODO Add some animation? *)
              self#read_update
          | Your_turn ->
              (* TODO Animation. *)
              (* TODO Center the camera on the player (how?) *)
              (* camera#set_position (data#actual_player) *)
              self#ack_update u ;
              self#read_update
          | _ ->
              self#ack_update u ;
              self#read_update
        end
    | None -> ()
    end

  method private process_animation =
    match current_animation with
    | Moving_unit (u,path) ->
        if frame_counter + 1 >= walking_time then
        begin
          frame_counter <- 0 ;
          match path with
          | [] -> current_animation <- Pause 10
          | e :: r -> current_animation <- Moving_unit (u, r)
        end
        else self#frame_incr
    | Attack ->
        if frame_counter + 1 >= attack_time
        then begin
          current_animation <- Nothing ;
          camera#cursor#set_state Cursor.Idle
        end
        else self#frame_incr
    | Boom _ ->
        if frame_counter + 1 >= boom_time
        then current_animation <- Nothing
        else self#frame_incr
    | Pause 0 -> current_animation <- Nothing
    | Nothing ->
        Mutex.unlock data#mutex;
        Thread.yield ()
    | Pause i ->
        if frame_counter >= i then current_animation <- Nothing
        else self#frame_incr

  method update =
    Mutex.unlock data#mutex;
    Mutex.lock data#mutex;
    if current_animation = Nothing then self#read_update ;
    self#process_animation

  method faster =
    speed <- match speed with
      | Slow   -> Normal
      | Normal -> Fast
      | Fast   -> FFast
      | FFast  -> FFast

  method slower =
    speed <- match speed with
      | Slow   -> Slow
      | Normal -> Slow
      | Fast   -> Normal
      | FFast  -> Fast

  method speed =
    match speed with
    | Slow   -> "slow"
    | Normal -> "normal"
    | Fast   -> "fast"
    | FFast  -> "ffast"

  method unit_position u =
    match current_animation with
    | Moving_unit (soldier, e :: n :: _) when soldier = u ->
        let fc = min frame_counter walking_time in
        (* Beware of magic numbers *)
        let o =
          (float_of_int fc) /. (float_of_int walking_time) *. 50.
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

  method burst_position =
    match current_animation with
    | Boom pos -> Some pos
    | _ -> None

end
