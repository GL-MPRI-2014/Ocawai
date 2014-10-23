class item : icon:string -> text:string -> action:(unit -> unit) -> object

  method icon : string

  method text : string

  method action : unit

end

class menu : title:string -> items:(item list) -> object

  method draw : #OcsfmlGraphics.render_target -> unit

  method select_up : unit
  method select_down : unit

  method selected : item

end
