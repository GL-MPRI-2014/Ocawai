(* TODO : Functorize this module to take an interval as an argument, such 
 * that positions will never get out of this interval *)

type t

val create : int * int -> t

val topair : t -> int * int

(** clamp p pmin pmax returns the position p clamped to the square delimited
  * by pmin and pmax *)
val clamp : t -> t -> t -> t

val left : t -> t

val right : t -> t

val down : t -> t

val up : t -> t

(** add p1 p2 returns the position p1 + p2 *)
val add : t -> t -> t

(** diff p1 p2 returns the position p1 - p2 *)
val diff : t -> t -> t

(** square p1 p2 returns the list of positions in the square :
  *      p1 x  x 
  *      x  x  x
  *      x  x  p2 
  * including p1 and p2, from left to right, top to bottom *)
val square : t -> t -> t list

(** circle c r returns the circle (for the norm |(x,y)| = |x| + |y|)
  * of center c and radius r *)
val circle : t -> int -> t list

(** Same as circle, but returns the ball *)
val filled_circle : t -> int -> t list

(** neighbours l returns the list containing the neighbours of the positions
  * of l, without duplicates *)
val neighbours : t list -> t list
