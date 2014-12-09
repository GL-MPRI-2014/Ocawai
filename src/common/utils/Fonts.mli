(** Used to load fonts inteligently *)

(** Exception raised when font was not found or not readable. *)

exception Cant_load_font of string

(** @return a [font] from its name, a [string]. Looks either in current
    sub-folders or in /usr/share/GL_2014.
*)
val load_font : string -> OcsfmlGraphics.font
