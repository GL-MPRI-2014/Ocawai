open OcsfmlGraphics
open Utils
open GuiTools

open Manager

type credits =
  | OCAWAI
  | Section of string * name list
  | Thank_you
  | Tips
  | Break
and name = string

class state = object(self)

  inherit State.state as super

  val font = Fonts.load_font "FreeSans.ttf"
  val bold = Fonts.load_font "FreeSansBold.ttf"

  val mutable off = 0.

  val mutable last_update = Unix.gettimeofday ()

  val mutable finished = false

  val mutable particles = new ParticleManager.particle_manager manager#window

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
      | Break ->
          let (_,h) = foi2D manager#window#get_size in
          top := !top +. h /. 3.
    in List.iter aux seq ;
    !top

  method render window =

    particles#update;

    let color = Color.rgb 10 10 10 in
    window#clear ~color ();

    particles#render;

    let (sx, sy) = Utils.foi2D window#get_size in

    if Random.int 90 <= 1 && not finished then begin
      let position = (Random.float sx, Random.float (sy /. 2.5)) in
      Booms.boom_circle particles
        (Random.float 200. +. 700.)
        position
        (Booms.random_color ())
        100
    end;

    let yellow = OcsfmlGraphics.Color.rgb 220 220 20 in

    Booms.continuous_fountain particles (0., sy) (-1.) 0.25 yellow;

    Booms.continuous_fountain particles (sx, sy) (4.141592) 0.25 yellow;

    if finished then 
      Booms.continuous_flower particles (sx /. 2., sy);

    let height = self#credits [
      OCAWAI ;
      Section ("Textures Hardcoding",
        ["Sheeft"]
      ) ;
      Section ("Fireworks",
        ["VLanvin"]
      ) ;
      Section ("Konami Snake",
        ["Artymort";"Sheeft"]
      ) ;
      Section ("Random Cartography",
        ["dbusatto"]
      ) ;
      Section ("Increasing Sloccount Rating",
        ["Saroupille"]
      ) ;
      Section ("Clickodromization",
        ["Nobody... right?"]
      ) ;
      Section ("Hiding Everything Everywhere",
        ["teoule17"]
      ) ;
      Section ("Number Crushing",
        ["MetaMetaMath"]
      ) ;
      Section ("The Man with Two Names",
        ["m-legrand"; "Nilexys"]
      ) ;
      Section ("Minimap Mipmapping",
        ["VLanvin"]
      ) ;
      Section ("Imperial March",
        ["tbazin";"Artymort"]
      ) ;
      Section ("Star Crafting",
        ["OlivierMarty"]
      ) ;
      Section ("Parcel Bombing",
        ["Paul-Gallot";"Mazzocchi";"juliengrange"]
      ) ;
      Break ;
      Section ("And for supporting us and our awful jokes...",
        []
      );
      Section ("... special thanks to:",
        ["dbaelde"; "ngrosshans"]
      ) ;
      Thank_you ;
      Tips
    ] in

    let dt = Unix.gettimeofday () -. last_update in
    last_update <- Unix.gettimeofday ();
    (* Increasing offset while it has meaning to do so *)
    if off < sy /. 3. +. height
      then off <- off +. (50. *. dt)
    else
      finished <- true;

    window#display

end
