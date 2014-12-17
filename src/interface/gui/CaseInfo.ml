open OcsfmlGraphics
open Utils
open GuiTools

let font = Fonts.load_font "FreeSans.ttf"


let width  = 220.
let height = 150.

class case_info = object(self)

  method draw : 'a. (#render_target as 'a) ->
  (string -> (float * float) -> unit) -> Unit.t option -> string -> Tile.t ->
  unit =
    fun target drawer u chara tile ->
      let (w,h) = foi2D target#get_size in
      let x = 10.
      and y = h -. 160. in
      new rectangle_shape
        ~position:(x,y)
        ~size:(width,height)
        ~fill_color:(Color.rgba 255 255 255 240)
        ()
      |> target#draw ;
      (* Unit information *)
      begin match u with
      | Some u ->
          drawer (chara ^ "_" ^ u#name) (30.,h-.140.) ;
          (* Name *)
          rect_print target u#name font (Color.rgb 33 33 33) (Pix 15) (Pix 2)
            Left
            { left = 50. ; top = h -. 150. ; width = 170. ; height = 50. };
          (* Player ID *)
          rect_print target ("#" ^ (string_of_int u#player_id)) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Right
            { left = 50. ; top = h -. 150. ; width = 170. ; height = 50. } ;
          (* Life *)
          drawer "life" (25.,h-.115.) ;
          rect_print target
            ((string_of_int u#hp) ^ "/" ^ (string_of_int u#life_max)) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Left
            { left = 37. ; top = h -. 125. ; width = 60. ; height = 50. };
          (* Move Range *)
          drawer "move_range" (110.,h-.115.) ;
          rect_print target (string_of_int u#move_range) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Left
            { left = 125. ; top = h -. 125. ; width = 10. ; height = 50. };
          (* Attack Range *)
          drawer "attack_range" (150.,h-.115.) ;
          rect_print target
            ((string_of_int u#min_attack_range)
              ^ "-" ^ (string_of_int u#attack_range))
            font (Color.rgb 77 77 77) (Pix 15) (Pix 2) Left
            { left = 165. ; top = h -. 125. ; width = 30. ; height = 50. }
      | None ->
          rect_print target "No unit there" font (Color.rgb 99 99 99)
            (Pix 15) (Pix 2)
            Center
            { left = 30. ; top = h -. 150. ; width = 170. ; height = 50. }
      end;
      (* Building information *)
      (* TODO: After merge *)
      rect_print target "Buildings don't exist..." font (Color.rgb 99 99 99)
        (Pix 15) (Pix 2)
        Center
        { left = 30. ; top = h -. 100. ; width = 170. ; height = 50. };
      (* Tile information *)
      (* TODO *)
      (* let tileset = TilesetLibrary.get_tileset tileset_library "tileset" in
      drawer (Tile.get_name tile) (30.,h-.50.) ; *)
      rect_print target (Tile.get_name tile) font (Color.rgb 99 99 99)
        (Pix 15) (Pix 2)
        Center
        { left = 30. ; top = h -. 50. ; width = 170. ; height = 50. }

end
