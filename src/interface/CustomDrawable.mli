
class virtual drawable : object 

  method virtual draw : target:OcsfmlGraphics.render_target -> 
    position:float*float -> size:float*float -> rotation:float ->
    origin:float*float -> color:OcsfmlGraphics.Color.t -> unit -> unit

end


class basic_texture : string -> object

  inherit drawable 

  method draw : target:OcsfmlGraphics.render_target -> 
    position:float*float -> size:float*float -> rotation:float ->
    origin:float*float -> color:OcsfmlGraphics.Color.t -> unit -> unit

end
