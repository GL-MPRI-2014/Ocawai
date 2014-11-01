(**
   Doubly chained list module used in music generation.
   Implements constant time insert in head AND tail.

   Represents the background, lower-level implementation for the Tile module.

   Based on the TPTM model developed by P. Hudak and
   D. Janin, code inspired by Theis Bazin's work on this model.
   @author "Theis Bazin" @author "Mathias Sable Meyer"
*)

type time
type event
type t

(** {2 Base DLists} *)

(**
   Delay to sync various tiles
*)
val sync : time -> t

(**
   Neutral element for the Tiled product
*)
val zero : t

(**
   Compares t to zero
*)
val isZero : t -> bool

(**
   Encapsulate an event into a singleton, that is a single element list
*)
val return : event -> t

(** {2 DList operators} *)

(**
   Infix operation for concatenation
   
   Eventually this infix operator should be modified, we wanted ::: but somehow
   this is not possible ...
*)
val (/::/) : t -> t -> t

(** {2 Normalization functions} *)

(**
   Split the abstract tree and yields its head and tail with respect to the time
*)
val headTail : t -> t * t

(** {2 Testing functions} *) 

(**
   Builds the tile containing all events in the list in sequence
*)
val fromList : event list -> t
