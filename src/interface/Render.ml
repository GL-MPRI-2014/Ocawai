open OcsfmlGraphics

let texture_library = TextureLibrary.create ()


let render_map (target : #OcsfmlGraphics.render_target) (map : Map.t) = ()


let () = 
  TextureLibrary.load_directory texture_library "resources/textures/"
