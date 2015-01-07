open OcsfmlGraphics

type t = {textures : render_texture array; size : (int * int)}

let blur_shader = new shader 
  ~vertex:(ShaderSource.file "src/interface/amaze/bloom.vert")
  ~fragment:(ShaderSource.file "src/interface/amaze/blur.frag") ()

let filter_shader = new shader
  ~vertex:(ShaderSource.file "src/interface/amaze/bloom.vert")
  ~fragment:(ShaderSource.file "src/interface/amaze/filter.frag") ()

let add_shader = new shader
  ~vertex:(ShaderSource.file "src/interface/amaze/bloom.vert")
  ~fragment:(ShaderSource.file "src/interface/amaze/add.frag") ()

let () = 
  blur_shader#set_current_texture "texture";
  filter_shader#set_current_texture "texture";
  add_shader#set_current_texture "texture"


let create (x,y) = 
  {textures = 
    [|new render_texture x y;
      new render_texture x y|];
   size = (x,y)}

let filter (sys:t) (source : render_texture) = 
  sys.textures.(0)#clear ();
  new sprite ~texture:source#get_texture ~position:(0.,0.) ()
  |> sys.textures.(0)#draw ~shader:filter_shader;
  sys.textures.(0)#display

let blur (sys:t) radius =
  sys.textures.(1)#clear();
  blur_shader#set_parameter2 "pix_size" 
    (radius /. (float_of_int (fst sys.size)))
    (radius /. (float_of_int (snd sys.size)));
  blur_shader#set_parameter2 "dir" 1. 0.;
  new sprite ~texture:sys.textures.(0)#get_texture ~position:(0.,0.) ()
  |> sys.textures.(1)#draw ~shader:blur_shader;
  sys.textures.(1)#display;
  sys.textures.(0)#clear();
  blur_shader#set_parameter2 "dir" 0. 1.;
  new sprite ~texture:sys.textures.(1)#get_texture ~position:(0.,0.) ()
  |> sys.textures.(0)#draw ~shader:blur_shader;
  sys.textures.(0)#display

let add (sys:t) (source : render_texture) (target : #render_target) =
  add_shader#set_texture "blurred" sys.textures.(0)#get_texture;
  new sprite ~texture:source#get_texture ~position:(0.,0.) ()
  |> target#draw ~shader:add_shader
 
let blooming (sys:t) (source : render_texture) (target : #render_target) =
  filter sys source;
  blur sys 4.;
  blur sys 2.;
  blur sys 1.;
  add sys source target
  
