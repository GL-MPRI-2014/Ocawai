class sliced_texture : string -> upcut:int -> downcut:int -> 
  rightcut:int -> leftcut:int -> object

  inherit CustomDrawable.drawable 

  method default_size : int * int

  (* Note : rotation/origin/color are unused for now *)
  method draw : target:OcsfmlGraphics.render_target -> 
    ?position:float*float -> ?size:float*float -> ?rotation:float ->
    ?origin:float*float -> ?color:OcsfmlGraphics.Color.t -> 
    ?scale:float*float -> ?blend_mode:OcsfmlGraphics.blend_mode -> 
      unit -> unit

end
