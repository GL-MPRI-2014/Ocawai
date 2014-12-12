open OcsfmlGraphics

let clk = new OcsfmlSystem.clock 

let framecount = ref 0

let current_fps = ref 0

let font = Fonts.load_font "digit.ttf"

let display (target : #render_target) =
  incr framecount;
  if OcsfmlSystem.Time.as_seconds clk#get_elapsed_time >= 1. then begin
    ignore clk#restart;
    current_fps := !framecount;
    framecount := 0
  end;
  new text ~string:(string_of_int !current_fps)
     ~font
     ~character_size:42
     ~color:Color.blue ()
  |> target#draw;


