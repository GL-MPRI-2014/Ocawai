open OcsfmlGraphics

exception Unknown_texture of string

type t = (string, texture) Hashtbl.t


let create () = Hashtbl.create 13

module TextureLoadingLog = Log.Make (struct let section = "Texture" end)

let load_texture lib path = 
  let i,i' = String.rindex path '.', String.rindex path '/' in 
  let name = String.sub path (i'+1) (i-i'-1) in 
  let ext  = String.sub path (i+1) (String.length path - i - 1) in
  if ext = "png" then begin 
    let tex = new texture (`File path) in
    tex#set_smooth true;
    TextureLoadingLog.infof "[stored] %s" name;
    Hashtbl.add lib name tex
  end


let rec load_recursively lib prefix path =
  if Sys.is_directory (prefix ^ path) then begin
    let children = Sys.readdir (prefix ^ path ^ "/") in 
    Array.iter (load_recursively lib (prefix ^ path ^ "/")) children
  end else
    load_texture lib (prefix ^ path)


let load_directory lib dir = 
  let children = Sys.readdir dir in 
  Array.iter (load_recursively lib dir) children


let get_texture lib name = 
  try 
    Hashtbl.find lib name 
  with 
    |Not_found ->
      TextureLoadingLog.errorf "Couldn't retreive texture %s" name;
      raise (Unknown_texture name)

