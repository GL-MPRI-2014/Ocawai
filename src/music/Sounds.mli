(**
   A simple tool to play in game sounds.

   The mli is quite minimalistic, but the implementation will load every
   sound into a buffer before playing it, this is not juste a renaming of
   some ocsfml.audio function.
*)


(** {2 Exceptions} *)

(**
  The exception of unknown sound. They have to be stored into
  resource/sound and have a wav extention in order to be properly loaded.
*)
exception Unknown_sound of string

(**
  This expression is raised whenever the file you are trying to load does not
  match a recognised sound file extention (Wav for the moment).
*)
exception Not_a_valid_sound_file of string

(** {2 Management functions} *)

(**
  Initiiate sound managing : loads the various sounds !
*)
val load_sounds : unit -> unit

(**
  Give the function the name of a buffered sound, and it will play it as
  efficiently as possible.
  Might raise Unknown_sound of string if the sound has not been loaded yet.
*)
val play_sound : string -> unit
