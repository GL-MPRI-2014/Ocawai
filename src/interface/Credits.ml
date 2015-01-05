open OcsfmlGraphics
open Utils
open GuiTools

open Manager

type credits =
  | OCAWAI
  | Section of string * name list
  | Thank_you
  | Tips
and name = string

class state = object(self)

  inherit State.state as super

  val font = Fonts.load_font "FreeSans.ttf"
  val bold = Fonts.load_font "FreeSansBold.ttf"

  val mutable off = 0.

  method handle_event e =
    OcsfmlWindow.Event.(
      match e with
      | KeyPressed { code = OcsfmlWindow.KeyCode.Escape ; _ }
      | KeyPressed { code = OcsfmlWindow.KeyCode.Q ; _ } ->
          manager#pop
      | _ -> ()
    )

  method private print s b size top =
    let window = manager#window in
    let (w,h) = foi2D window#get_size in
    rect_print window s (if b then bold else font) Color.white
      (Pix size) (Pix 5) Center
      { left = 10.; top = h -. 50. -. off +. top; width = w -. 20.; height = h }

  method private title s top =
    self#print s true 30 top

  method private text s top =
    self#print s false 25 top

  method private credits seq =
    let top = ref 0. in
    let aux = function
      | OCAWAI ->
          self#print "OCAWAI" true 100 !top ;
          top := !top +. 150.
      | Thank_you ->
          top := !top +. 50. ;
          self#print "Thank you!" true 50 !top
      | Section (title,names) ->
          self#title title !top ;
          top := !top +. 34. ;
          List.iter (fun s -> self#text s !top ; top := !top +. 28.) names ;
          top := !top +. 20.
      | Tips ->
          top := !top +. 150. ;
          self#print "By the way, press escape to quit..." false 15 !top ;
          top := !top +. 20.
    in List.iter aux seq

  method render window =

    off <- off +. 1.3 ;

    let color = Color.rgb 10 10 10 in
    window#clear ~color ();

    self#credits [
      OCAWAI ;
      Section ("Textures Hardcoding",
        ["Sheeft"]
      ) ;
      Section ("Fireworks",
        ["Paul-Gallot"]
      ) ;
      Section ("Minimap Mipmapping",
        ["VLanvin"]
      ) ;
      Section ("Konami Snake",
        ["VLanvin";"Artymort";"Sheeft"]
      ) ;
      Section ("Clickodromization",
        ["Nobody... right?"]
      ) ;
      Section ("Imperial March",
        ["TBazin";"Artymort"]
      ) ;
      Section ("Parcel Bombing",
        ["Mazzocchi";"juliengrange"]
      ) ;
      Thank_you ;
      Tips
    ] ;

    window#display

end
