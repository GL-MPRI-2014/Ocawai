open OcsfmlGraphics

exception Unknown_texture of string

type drawable = TSet of Tileset.tileset | Tex of texture

type t = (string, drawable) Hashtbl.t


let create () = Hashtbl.create 13


let load_texture lib path = 
  let i,i' = String.rindex path '.', String.rindex path '/' in 
  let name = String.sub path (i'+1) (i-i'-1) in 
  let ext  = String.sub path (i+1) (String.length path - i - 1) in
  if ext = "png" then begin 
    let tex = new texture (`File path) in
    print_endline ("  [\027[32mstored\027[0m] " ^ name);
    Hashtbl.add lib name (Tex tex)
  end else if ext = "set" then begin
    let tex = new texture (`File path) in
    let cfg = (String.sub path 0 i) ^ ".cfg" in
    let set = new Tileset.tileset tex cfg in
    print_endline ("  [\027[32mstored\027[0m] " ^ name);
    Hashtbl.add lib name (TSet set)
  end


let rec load_recursively lib prefix path =
  if Sys.is_directory (prefix ^ path) then begin
    let children = Sys.readdir (prefix ^ path ^ "/") in 
    Array.iter (load_recursively lib (prefix ^ path ^ "/")) children
  end else
    load_texture lib (prefix ^ path)


let load_directory lib dir = 
  print_endline "Loading textures :";
  let children = Sys.readdir dir in 
  Array.iter (load_recursively lib dir) children;
  print_endline "All textures loaded"


let get_texture lib name = 
  try 
    Hashtbl.find lib name 
  with 
    |Not_found -> raise (Unknown_texture name)

let assert_texture d = 
  match d with
  |Tex(texture) -> texture
  |_ -> assert false


