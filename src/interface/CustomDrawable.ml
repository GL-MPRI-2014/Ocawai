open OcsfmlGraphics
open OcsfmlWindow

class virtual drawable = object
  
  method virtual draw : target:render_target -> 
    ?position:float*float -> ?size:float*float -> ?rotation:float ->
    ?origin:float*float -> ?color:Color.t -> ?scale:float*float -> 
    ?blend_mode:blend_mode -> unit -> unit

  method virtual default_size : int * int

end


class basic_texture file = object

  inherit drawable

  val sf_texture = new texture (`File file)

  val mutable tex_size = (0.,0.)

  initializer
    sf_texture#set_smooth true;
    tex_size <- Utils.foi2D sf_texture#get_size


  method draw ~target ?position:(position=(0.,0.)) 
              ?size:(size=tex_size) ?rotation:(rotation=0.) 
              ?origin:(origin=(0.,0.)) ?color:(color=Color.white) 
              ?scale:(scale=(1.,1.)) ?blend_mode:(blend_mode = BlendAlpha) () =
    new sprite ~texture:sf_texture ~position 
               ~scale:((fst size) *. (fst scale) /. (fst tex_size),
                       (snd size) *. (snd scale) /. (snd tex_size))
               ~rotation ~origin ~color ()
    |> target#draw ~blend_mode

  method default_size = sf_texture#get_size

end




