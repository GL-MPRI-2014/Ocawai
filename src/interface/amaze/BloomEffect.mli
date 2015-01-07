open OcsfmlGraphics

type t

val create : (int * int) -> t

val blooming : t -> render_texture -> #render_target -> unit
