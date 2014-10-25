open OcsfmlGraphics
open OcsfmlWindow

class virtual drawable = object
  
  method virtual draw : target:render_target -> 
    position:float*float -> size:float*float -> rotation:float ->
    origin:float*float -> color:Color.t -> unit -> unit

end


class basic_texture file = object

  inherit drawable

  val sf_texture = new texture (`File file)

  val mutable tex_size = (0.,0.)

  initializer
    tex_size <- Utils.foi2D sf_texture#get_size

  method draw ~target ~position ~size 
              ~rotation ~origin ~color () = 
    new sprite ~texture:sf_texture ~position 
               ~scale:((fst size) /. (fst tex_size),
                       (snd size) /. (snd tex_size))
               ~rotation ~origin ~color ()
    |> target#draw

end




