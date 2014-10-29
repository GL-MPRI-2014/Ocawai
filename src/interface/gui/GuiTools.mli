(** Gives tools for the GUI *)

(** Used for text alignment *)
type alignment = Center | Left | Right

(** Defines a rectangle on the screen
  * [(x,y,w,h)] defines a rectangle whose top-left corner is at [(x,y)] of
  * width [w] and height [h] *)
type rect = int*int*int*int

(** [rect_print text font alignment rectangle] *)
val rect_print : string -> OCsfmlGraphics.font -> alignment -> rect -> unit
