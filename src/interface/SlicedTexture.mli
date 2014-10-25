class sliced_texture : string -> upcut:int -> downcut:int -> 
  rightcut:int -> leftcut:int -> object

  inherit CustomDrawable.drawable 

  method draw : target:OcsfmlGraphics.render_target -> 
    position:float*float -> size:float*float -> rotation:float ->
    origin:float*float -> color:OcsfmlGraphics.Color.t -> unit -> unit

end
