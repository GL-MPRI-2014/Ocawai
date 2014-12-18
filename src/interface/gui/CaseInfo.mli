(* Prints tile/building/unit information *)
class case_info : object

  method draw : #OcsfmlGraphics.render_target ->
    (string -> (float * float) -> unit) ->
    (string -> (float * float) -> unit) ->
    Unit.t option ->
    string ->
    Building.t option ->
    string ->
    Tile.t ->
    unit

end
