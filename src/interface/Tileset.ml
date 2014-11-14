exception Tileset_error of string

type tile_cfg = {
  tile_size : int; 
  width     : int; 
  height    : int; 
  textures  : string list}

let parse_tileset_cfg file = 
  let input = open_in file in 
  let size   = ref (-1) in 
  let width  = ref (-1) in
  let height = ref (-1) in
  let texts  = ref [] in
  let rec parse_aux () = 
    try begin
      let s = input_line input in
      let l = Str.split (Str.regexp "[\t ]+") s in 
      match l with
      |"size"   :: "=" :: v :: _ -> size := (int_of_string v)
      |"width"  :: "=" :: v :: [] -> width := (int_of_string v)
      |"height" :: "=" :: v :: [] -> height := (int_of_string v)
      |"textures" :: "=" :: vlist -> 
          let l' = Str.split (Str.regexp ",") (String.concat "" vlist) in 
          List.iter (fun t -> texts := t :: !texts) l'
      |[] -> ()
      |_  -> raise (Tileset_error ("Syntax error in " ^ file ^ " : " ^ s))
    end; parse_aux ()
    with
      |End_of_file -> ()
  in
  parse_aux ();
  {tile_size = !size; width = !width; height = !height; textures = !texts}


class tileset texture config = 

  let parse_result = parse_tileset_cfg config in 

  object(self)

    val size = parse_result.tile_size

    val width = parse_result.width

    val height = parse_result.height

    val coordinates = Hashtbl.create 10 

    initializer
      List.iteri (fun i s ->
        let cw = i mod width in 
        let ch = i / width in 
        Hashtbl.add coordinates s (cw * size, ch * size)
      ) (List.rev parse_result.textures)

    method tile_size = size

    method texture : OcsfmlGraphics.texture = texture

    method texture_coords s = 
      try
        Hashtbl.find coordinates s
      with 
        |Not_found -> raise (Tileset_error ("Tile not found : " ^ s))

    method texture_rect s = 
      let (cx, cy) = self#texture_coords s in 
      OcsfmlGraphics.({left = cx; top = cy; width = size; height = size})

end


