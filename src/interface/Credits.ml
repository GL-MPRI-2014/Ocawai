open OcsfmlGraphics
open Utils
open GuiTools

open Manager

class state = object(self)

  inherit State.state as super

  val font = Fonts.load_font "FreeSans.ttf"
  val bold = Fonts.load_font "FreeSansBold.ttf"

  val mutable off = 0.
  val mutable top = 0.

  method handle_event e =
    OcsfmlWindow.Event.(
      match e with
      | KeyPressed { code = OcsfmlWindow.KeyCode.Escape ; _ }
      | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; _ } ->
          manager#pop
      | _ -> ()
    )

  method private print s b size =
    let window = manager#window in
    let (w,h) = foi2D window#get_size in
    rect_print window s (if b then bold else font) Color.white
      (Pix size) (Pix 5) Center
      { left = 10.; top = h -. 50. -. off +. top; width = w -. 20.; height = h };
    top <- top +. (float_of_int size *. 2.5)

  method private title s =
    self#print s true 30

  method private text s =
    self#print s false 25

  method render window =

    off <- off +. 0.8 ;

    let color = Color.rgb 10 10 10 in
    window#clear ~color ();

    (* Ugly *)
    top <- 0. ;

    self#print "OCAWAI" true 100 ;

    self#title "Textures Hardcoding" ;
    self#text  "Dada" ;
    self#text  "Dudu" ;
    top <- top +. 20. ;

    self#title "Fireworks" ;
    self#text  "Who did that?" ;
    top <- top +. 20. ;

    self#title "Minimap Mipmapping" ;
    self#text  "VLanvin" ;
    top <- top +. 50. ;

    self#print "Thank you!" true 50 ;

    window#display

end
