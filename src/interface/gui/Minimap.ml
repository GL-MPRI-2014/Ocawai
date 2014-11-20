open OcsfmlGraphics

type mini_tile = Forest | Mountain | Plain | Water

let player_colors = [|
  Color.rgb 255 0 0;
  Color.rgb 0 255 0;
  Color.rgb 0 0 255;
  Color.rgb 255 255 0 |]

class minimap def width height = object(self)

  val map = Array.make_matrix def def Plain

  val player_map = Array.make_matrix def def None

  method private compute_players players = 
    let majority_map = Array.make_matrix def def [||] in
    let n = List.length players in 
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        majority_map.(i).(j) <- Array.create n 0
      done;
    done;
    let act_player = ref 0 in
    List.iter (fun p ->
      List.iter (fun u ->
        let (px,py) = Position.topair u#position in
        let foi = float_of_int in
        let (px', py') = Utils.iof2D (
          ((foi px) /. (foi (width  - 1))) *. (foi (def - 1)),
          ((foi py) /. (foi (height - 1))) *. (foi (def - 1)))
        in 
        majority_map.(px').(py').(!act_player) <-
          majority_map.(px').(py').(!act_player) + 1
      ) p#get_army;
      incr act_player
    ) players;
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let maxp = ref (-1) in
        let maxu = ref 0    in 
        for k = 0 to n-1 do 
          if majority_map.(i).(j).(k) > !maxu then begin
            maxu := majority_map.(i).(j).(k);
            maxp := k
          end
        done;
        if !maxp <> -1 then 
          player_map.(i).(j) <- Some !maxp
      done;
    done

  method private compute_battlefield battlefield = 
    let majority_map = Array.make_matrix def def [||] in
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        majority_map.(i).(j) <- Array.create 4 0
      done;
    done;
    for i = 0 to width - 1 do
      for j = 0 to height - 1 do
        let foi = float_of_int in
        let (px, py) = Utils.iof2D (
          ((foi i) /. (foi (width  - 1))) *. (foi (def - 1)),
          ((foi j) /. (foi (height - 1))) *. (foi (def - 1)))
        in 
        let pos = Position.create (i,j) in
        match Tile.get_name (Battlefield.get_tile battlefield pos) with
        |"forest" -> 
            majority_map.(px).(py).(0) <-
              majority_map.(px).(py).(0) + 1
        |"mountain" -> 
            majority_map.(px).(py).(1) <-
              majority_map.(px).(py).(1) + 1
        |"plain" -> 
            majority_map.(px).(py).(2) <-
              majority_map.(px).(py).(2) + 1
        |"water" -> 
            majority_map.(px).(py).(3) <-
              majority_map.(px).(py).(3) + 1
        | _ -> ()
      done;
    done;
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let maxt = ref (-1) in
        let maxn = ref (-1) in 
        for k = 0 to 3 do 
          if majority_map.(i).(j).(k) > !maxn then begin
            maxn := majority_map.(i).(j).(k);
            maxt := k
          end
        done;
        match !maxt with
        |0 -> map.(i).(j) <- Forest
        |1 -> map.(i).(j) <- Mountain
        |2 -> map.(i).(j) <- Plain
        |3 -> map.(i).(j) <- Water
        |_ -> assert false
      done;
    done

  method compute battlefield (players : Player.logicPlayer list) = 
    self#compute_battlefield battlefield;
    self#compute_players players

  method private add_rectangle vao pos size color = 
    vao#append (mk_vertex ~position:pos ~color ());
    vao#append (mk_vertex ~position:(Utils.addf2D pos (fst size, 0.)) ~color ());
    vao#append (mk_vertex ~position:(Utils.addf2D pos size) ~color ());
    vao#append (mk_vertex ~position:(Utils.addf2D pos (0., snd size)) ~color ())

  val vao = new vertex_array ~primitive_type:Quads []

  method draw : 'a. (#render_target as 'a) -> Cursor.cursor -> unit = 
    fun target cursor ->
    let foi = float_of_int in
    let ratio = 200. /. (foi def) in
    self#add_rectangle vao (4.,4.) (208.,208.) (Color.rgb 200 200 200);
    for i = 0 to def - 1 do
      for j = 0 to def - 1 do
        let fill_color = 
          match map.(i).(j) with
          |Forest -> Color.rgb 0 100 0
          |Mountain -> Color.rgb 50 140 0
          |Plain -> Color.rgb 80 180 80
          |Water -> Color.rgb 50 50 220
        in
        self#add_rectangle vao ((8.+.(foi i)*.ratio), (8.+.(foi j)*.ratio))
          (ratio,ratio) fill_color;
        match player_map.(i).(j) with
        |None -> ()
        |Some(p) ->
          let alpha = (sin (Unix.gettimeofday () *. 3.) +. 1.)/. 2. in 
          let alpha = int_of_float (100. *. alpha) + 50 in
          self#add_rectangle vao ((8.+.(foi i)*.ratio), (8.+.(foi j)*.ratio))
            (ratio,ratio) (Color.rgba 255 255 255 alpha);
          self#add_rectangle vao ((9.+.(foi i)*.ratio), (9.+.(foi j)*.ratio))
            (ratio-.2.,ratio-.2.) player_colors.(p);
      done;
    done;
    let (px, py) = Position.topair cursor#position in
    let (px', py') = Utils.iof2D (
      ((foi px) /. (foi (width  - 1))) *. (foi (def - 1)),
      ((foi py) /. (foi (height - 1))) *. (foi (def - 1)))
    in 
    self#add_rectangle vao ((8.+.(foi px')*.ratio), (8.+.(foi py')*.ratio))
      (ratio, ratio) (Color.rgba 255 255 255 180);
    target#draw vao;
    vao#clear

end
    
