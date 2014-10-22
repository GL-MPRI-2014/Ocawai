val iof2D : (float * float) -> (int * int)

val foi2D : (int * int) -> (float * float)

(* Usage : clamp2D (x,y) (minx, miny) (maxx, maxy) *)
val clamp2D : ('a * 'a) -> ('a * 'a) -> ('a * 'a) -> ('a * 'a)

val addf2D : (float * float) -> (float * float) -> (float * float)

val subf2D : (float * float) -> (float * float) -> (float * float)

val add2D : (int * int) -> (int * int) -> (int * int)

val sub2D : (int * int) -> (int * int) -> (int * int)

(* opt >? f applies f to s if opt = Some(s) or returns unit *)
val (>?) : 'a option -> ('a  -> unit) -> unit
