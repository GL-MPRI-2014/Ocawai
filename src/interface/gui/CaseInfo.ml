open OcsfmlGraphics

class case_info = object(self)

  method compute = ()

  method draw : 'a. (#render_target as 'a) -> Cursor.cursor -> unit =
    fun target cursor ->
    ()

end
