open OcsfmlGraphics

exception Cant_load_font of string

module FontsLog = Log.Make (struct let section = "Fonts" end)

let load_font s =
  let font = new font `None in
  if not (font#load_from_file ((Utils.base_path ()) ^ "fonts/" ^ s))
  then (FontsLog.error "%s" s; raise (Cant_load_font s));
  FontsLog.info "[loaded] %s" s;
  font
