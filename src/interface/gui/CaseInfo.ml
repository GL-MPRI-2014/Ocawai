open OcsfmlGraphics
open Utils
open GuiTools

let font = Fonts.load_font "FreeSans.ttf"

let width  = 220.
let height = 150.

class case_info = object(self)

  method draw : 'a. (#render_target as 'a) -> (string -> (float * float) -> unit) -> Unit.t option -> unit =
    fun target drawer u ->
      let (w,h) = foi2D target#get_size in
      new rectangle_shape
        ~position:(10.,h-.160.)
        ~size:(width,height)
        ~fill_color:(Color.rgba 255 255 255 240)
        ()
      |> target#draw ;
      (* Unit information *)
      match u with
      | Some u ->
          (* TODO Add character handling *)
          drawer ("blub_" ^ u#name) (30.,h-.140.) ;
          rect_print target u#name font (Color.rgb 33 33 33) (Pix 15) (Pix 2)
            Left
            { left = 50. ; top = h -. 150. ; width = 170. ; height = 50. };
          rect_print target "#3" font (Color.rgb 77 77 77) (Pix 15) (Pix 2)
            Right
            { left = 50. ; top = h -. 150. ; width = 170. ; height = 50. }
      | None ->
          rect_print target "No unit there" font (Color.rgb 99 99 99)
            (Pix 15) (Pix 2)
            Center
            { left = 30. ; top = h -. 150. ; width = 170. ; height = 50. }
      (* Building information *)
      (* TODO: After merge *)
      (* Tile information *)
      (* TODO *)

end
