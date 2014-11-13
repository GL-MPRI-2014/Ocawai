
type t = Tile.t array array

let create =
  Array.make_matrix

let get_tile m pos =
  let (x,y) = Position.topair pos in
  m.(x).(y)

let set_tile m pos tile =
  let (x,y) = Position.topair pos in
  m.(x).(y) <- tile

let tile_iter f m =
  Array.iter (Array.iter f) m

let tile_iteri f m =
  Array.iteri (fun x -> Array.iteri (fun y -> f (Position.create (x,y)))) m

let tile_filter f m =
  List.map fst (List.filter (fun a -> f (snd a)) (Array.fold_left (fun l e -> (Array.to_list e)@l) [] (Array.mapi (fun i ss_m -> (Array.mapi (fun j t -> (Position.create(i,j),t)) ss_m)) m)))

let tile_filteri f m =
  List.map fst (List.filter (fun a -> f (fst a) (snd a)) (Array.fold_left (fun l e -> (Array.to_list e)@l) [] (Array.mapi (fun i ss_m -> (Array.mapi (fun j t -> (Position.create(i,j),t)) ss_m)) m)))

let size m = (Array.length m,Array.length m.(0))

let in_range (bf : t) (pos : Position.t) : bool =
  let pmin = Position.create (0,0) in
  let pmax = Position.create (let w,h = size bf in w-1,h-1) in
  not (Position.out_of_bounds pos pmin pmax)

let to_string_off m off1 off2 =
  let (w,h) = size m in
  let tiles = Tile.create_list_from_config() in
  let offset = if List.length tiles < 256-off1 then off1 else 0 in
  if (offset+List.length tiles>256) then failwith("more than 255 tiles in config, to_string can't be applied") else
  let rec mempos e = function
  | n,[] -> failwith("tile not in config")
  | n,p::q when Tile.get_name p = Tile.get_name e -> n
  | n,p::q -> mempos e (n+1,q)
  in
  let s = String.init 
    (w*h) 
    (fun i ->
      let pos = Position.create (i/w,i mod w) in
      char_of_int(mempos (get_tile m pos) (offset,tiles))
    ) in
  let compress s =
    let rec aux = function
    | [],i when 0<=i && i < String.length s -> aux([(s.[i],1)],i+1)
    | (a,b)::q,i when 0<=i && i < String.length s -> 
      if a = s.[i] && b<255-off2 then 
        aux((a,b+1)::q,i+1) 
      else 
        aux((s.[i],1)::(a,b)::q,i+1)
    |l,i-> l
    in
    let li = List.rev (aux ([],0)) in
    let ss = String.init (2*List.length li) (fun i -> '?') in
    let rec list_to_string n = function
    | []->ss
    | (a,b)::q ->
      Bytes.set ss (2*n) a;
      Bytes.set ss (2*n+1) (char_of_int (b+off2));
      list_to_string (n+1) q
    in
    list_to_string 0 li
  in
  compress s

let create_from_string_off w h s_short off1 off2 =
  let decompress s =
    let rec string_to_list = function
    |i when 0 <=i && 2*i+1 < String.length s -> (s.[2*i],int_of_char s.[2*i+1] - off2)::(string_to_list (i+1))
    |i -> []
    in
    let li = string_to_list 0 in
    let size_new_string = List.fold_left (fun t e -> snd e + t) 0 li in
    let ss = String.init size_new_string (fun i -> '?') in
    let rec fill_string p = function
    | [] -> ss
    | (a,b)::q -> 
      for i = 0 to b-1 do 
        Bytes.set ss (p+i) a;
      done;
      fill_string (p+b) q
    in
    fill_string 0 li
  in
  let s = decompress s_short in
  let tiles = Tile.create_list_from_config() in
  let offset = if List.length tiles < 256-off1 then off1 else 0 in
  if (offset+List.length tiles>256) then failwith("more than 255 tiles in config, create_from_string can't be applied") else
  let m = create h w (List.hd tiles) in
  tile_iteri 
    (fun p _ -> 
      let i = let x,y = Position.topair p in x*w+y in
      set_tile m p (List.nth tiles (int_of_char s.[i] - offset))
    ) m;
  m

let to_string m = to_string_off m 97 48
let create_from_string w h s = create_from_string_off w h s 97 48
