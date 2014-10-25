
class virtual drawable : object 

  method virtual draw : target:OcsfmlGraphics.render_target -> 
    ?position:float*float -> ?size:float*float -> ?rotation:float ->
    ?origin:float*float -> ?color:OcsfmlGraphics.Color.t -> 
    ?scale:float*float -> ?blend_mode:OcsfmlGraphics.blend_mode -> 
      unit -> unit

  method virtual default_size : int * int

end


class basic_texture : string -> object

  inherit drawable 

  method draw : target:OcsfmlGraphics.render_target -> 
    ?position:float*float -> ?size:float*float -> ?rotation:float ->
    ?origin:float*float -> ?color:OcsfmlGraphics.Color.t -> 
    ?scale:float*float -> ?blend_mode:OcsfmlGraphics.blend_mode -> 
      unit -> unit

  method default_size : int * int

end
