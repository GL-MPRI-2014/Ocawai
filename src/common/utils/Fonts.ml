open OcsfmlGraphics

exception Cant_load_font of string

let load_font s =
  let font = new font `None in
  if not (font#load_from_file ("resources/fonts/" ^ s)) then
    if not (font#load_from_file ("/usr/share/GL_2014/fonts" ^ s)) then
      raise (Cant_load_font s);
  font
