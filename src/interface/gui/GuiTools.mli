(** Gives tools for the GUI *)

(** Used for text alignment *)
type alignment = Center | Left | Right

(** Abstracts ints to give sizes as pixels or points *)
type quantity = Pix of int | Pt of float

(** @return a quantity in pixels (as [int]).
  * For now it doesn't return the real interpretation of points
  * as I don't know how to get the ppi *)
val to_pixels : quantity -> int

(** [rect_print traget text font color size interline alignment rectangle]
  * prints [text] in [rectangle] ;
  * it supports multiline and in case the rectangle is too narrow,
  * the text is truncated (warning : it can even remain unprinted!) *)
val rect_print : #OcsfmlGraphics.render_target -> string ->
  OcsfmlGraphics.font -> OcsfmlGraphics.Color.t -> quantity -> quantity ->
  alignment -> (float OcsfmlGraphics.rect) -> unit
