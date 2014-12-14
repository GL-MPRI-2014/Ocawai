(* Prints tile/building/unit information *)
class case_info : object

  method compute : unit

  method draw : #OcsfmlGraphics.render_target -> (string -> (float * float) -> unit) -> unit

end
