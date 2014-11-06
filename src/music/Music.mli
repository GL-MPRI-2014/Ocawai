(**
   Defines the basic elements for music generation.

   Defines types ['a Music.t] and [Music.param] :
   the first one is a single note with parameters in ['a],
   the second is a type for parameters, one then builds
   music with the base type [Music.param Music.t]
*)

type 'a t

(**
   One specific type for ['a] in ['a t], used in other modules
*)
type param

(**
   The instantion of ['a t] used in other modules
*)
type event = param t

(**
   @return the length of an event
*)
val getDur : 'a t -> Time.t
