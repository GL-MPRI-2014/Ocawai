(** Handles textures *)

type t

exception Unknown_texture of string

val create : unit -> t

(** Recursively load the textures of a directory and its sub-directories *)
val load_directory : t -> string -> unit

val load_texture : t -> string -> unit

val get_texture : t -> string -> OcsfmlGraphics.texture
