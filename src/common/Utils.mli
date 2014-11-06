(** Some utility functions  *)

(** @return a pair of [int] from a pair of [float] *)
val iof2D : (float * float) -> (int * int)

(** @return a pair of [float] from a pair of [int] *)
val foi2D : (int * int) -> (float * float)

(** Usage : [clamp2D (x,y) (minx, miny) (maxx, maxy)] *)
val clamp2D : ('a * 'a) -> ('a * 'a) -> ('a * 'a) -> ('a * 'a)

(** @return the addition of pairs of [float] *)
val addf2D : (float * float) -> (float * float) -> (float * float)

(** @return the substraction of pairs of [float]  *)
val subf2D : (float * float) -> (float * float) -> (float * float)

(** @return the addition of pairs of [int]  *)
val add2D : (int * int) -> (int * int) -> (int * int)

(** @return the substraction of pairs of [int]  *)
val sub2D : (int * int) -> (int * int) -> (int * int)

(** [opt >? f] applies [f] to [s] if [opt = Some(s)] otherwise returns [()] *)
val (>?) : 'a option -> ('a  -> unit) -> unit

(** [shuffle l] return a new list with elements reorganized randomly *)
val shuffle : 'a list -> 'a list
