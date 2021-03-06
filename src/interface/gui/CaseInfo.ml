open OcsfmlGraphics
open Utils
open GuiTools

let font = Fonts.load_font "FreeSans.ttf"


let width  = 220.
let height = 150.

class case_info = object(self)

  method draw : 'a. (#render_target as 'a) ->
                (string -> (float * float) -> unit) ->
                (string -> (float * float) -> unit) ->
                (int * int) option ->
                bool ->
                Unit.t option ->
                string ->
                Building.t option ->
                string ->
                Tile.t ->
                unit =
    fun target drawer tile_drawer damage foggy u chara building b_chara tile ->
      let (w,h) = foi2D target#get_size in
      let x = 10.
      and y = h -. 160. in
      let uh_offset = if u = None
        then 50.
        else 0.
      in
      let bh_offset = if building = None
        then 50.
        else 0.
      in
      (* Damage estimation *)
      begin match damage with
      | Some (dmin,dmax) ->
          new rectangle_shape
            ~position:(x,y-.25.+.uh_offset+.bh_offset)
            ~size:(width,25.)
            ~fill_color:(Color.rgba 255 34 0 240)
            ()
          |> target#draw;
          rect_print target
            ("Estimated damage: "
            ^ (string_of_int dmin)
            ^ " - " ^ (string_of_int dmax))
            font Color.white (Pix 15) (Pix 2) Left
            { left = x+.3. ;
              top = y-.25.+.uh_offset+.bh_offset+.2. ;
              width = 214. ;
              height = 25. }
      | None -> ()
      end ;
      (* Is the case foggy? *)
      if foggy then begin
        new rectangle_shape
          ~position:(x,y-.25.+.uh_offset+.bh_offset)
          ~size:(width,25.)
          ~fill_color:(Color.rgba 190 170 170 240)
          ()
        |> target#draw ;
        rect_print target
          "Under the fog of war..."
          font Color.white (Pix 15) (Pix 2) Left
          { left = x+.3. ;
            top = y-.25.+.uh_offset+.bh_offset+.2. ;
            width = 214. ;
            height = 25. }
      end ;
      (* Case information background *)
      new rectangle_shape
        ~position:(x,y+.uh_offset+.bh_offset)
        ~size:(width,height-.uh_offset-.bh_offset)
        ~fill_color:(Color.rgba 210 230 255 240)
        ()
      |> target#draw ;
      (* Unit information *)
      begin match u with
      | Some u ->
          drawer (chara ^ "_" ^ u#name) (30.,h-.140.+.bh_offset) ;
          (* Name *)
          rect_print target u#name font (Color.rgb 33 33 33) (Pix 15) (Pix 2)
            Left
            { left=50. ; top=h-.150.+.bh_offset ; width=170. ; height=50. };
          (* Player ID *)
          rect_print target ("#" ^ (string_of_int u#player_id)) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Right
            { left=50. ; top=h-.150.+.bh_offset ; width=170. ; height=50. } ;
          (* Life *)
          drawer "life" (25.,h-.115.+.bh_offset) ;
          rect_print target
            ((string_of_int u#hp) ^ "/" ^ (string_of_int u#life_max)) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Left
            { left=37. ; top=h-.125.+.bh_offset ; width=60. ; height=50. };
          (* Move Range *)
          drawer "move_range" (110.,h-.115.+.bh_offset) ;
          rect_print target (string_of_int u#move_range) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Left
            { left=125. ; top=h-.125.+.bh_offset ; width=10. ; height=50. };
          (* Attack Range *)
          drawer "attack_range" (150.,h-.115.+.bh_offset) ;
          rect_print target
            ((string_of_int u#min_attack_range)
              ^ "-" ^ (string_of_int u#attack_range))
            font (Color.rgb 77 77 77) (Pix 15) (Pix 2) Left
            { left=165. ; top=h-.125.+.bh_offset ; width=30. ; height=50. }
      | None -> ()
      end ;
      (* Building information *)
      begin match building with
      | Some b ->
          drawer (b_chara ^ "_" ^ b#name) (30.,h-.87.) ;
          (* Name *)
          rect_print target b#name font (Color.rgb 33 33 33)
            (Pix 15) (Pix 2)
            Left
            { left = 50. ; top = h -. 95. ; width = 170. ; height = 50. } ;
          (* Player id *)
          let id = match b#player_id with
            | Some id -> "#" ^ (string_of_int id)
            | None -> "neutral"
          in
          rect_print target id font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Right
            { left = 50. ; top = h -. 95. ; width = 170. ; height = 50. } ;
          (* Income *)
          drawer "income" (25.,h-.62.) ;
          rect_print target
            (string_of_int b#income) font
            (Color.rgb 77 77 77)
            (Pix 15) (Pix 2)
            Left
            { left = 37. ; top = h -. 70. ; width = 60. ; height = 50. }
      | None -> ()
      end ;
      (* Tile information *)
      tile_drawer (Tile.get_name tile) (20.,h-.50.) ;
      rect_print target (Tile.get_name tile) font (Color.rgb 66 66 66)
        (Pix 15) (Pix 2)
        Left
        { left = 60. ; top = h -. 45. ; width = 170. ; height = 50. }

end
