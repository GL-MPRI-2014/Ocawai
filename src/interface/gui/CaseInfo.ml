open OcsfmlGraphics
open Utils
open GuiTools

let font = Fonts.load_font "FreeSans.ttf"

class case_info = object(self)

  method compute = ()

  method draw : 'a. (#render_target as 'a) -> (string -> (float * float) -> unit) -> unit =
    fun target drawer ->
      let (w,h) = foi2D target#get_size in
      new rectangle_shape
        ~position:(10.,h-.160.)
        ~size:(220.,150.)
        ~fill_color:(Color.rgba 255 255 255 240)
        ()
      |> target#draw ;
      (* Unit information *)
      drawer "blub_infantry" (30.,h-.140.)
        (* ~size:(14.,14.)
        ~position:(15.,h-.150.) *)
        (* () *) ;
      rect_print target "infantry" font (Color.rgb 33 33 33) (Pix 15) (Pix 2) Left
        { left = 50. ; top = h -. 150. ; width = 170. ; height = 50. };
      rect_print target "#3" font (Color.rgb 77 77 77) (Pix 15) (Pix 2) Right
        { left = 50. ; top = h -. 150. ; width = 170. ; height = 50. }
      (* Building information *)
      (* TODO: After merge *)
      (* Tile information *)
      (* TODO *)

end
