(**
   Doubly chained list module used in music generation.
   Implements constant time insert in head AND tail.

   Represents the background, lower-level implementation for the Tile module.

   Based on the TPTM model developed by P. Hudak and
   D. Janin, code inspired by Theis Bazin's work on this model.
   @author "Theis Bazin" @author "Mathias Sable Meyer"
*)

type time
type 'a event
type 'a t

(** {2 Base DLists} *)

(**
   Delay to sync various tiles
*)
val sync : time -> 'a t

(**
   Neutral element for the Tiled product
*)
val zero : 'a t

(**
   Compares t to zero
*)
val isZero : 'a t -> bool

(**
   Encapsulate an event into a singleton, that is a single element list
*)
val return : 'a event -> 'a t

(** {2 DList operators} *)

(**
   Infix operation for concatenation
*)
(*  
    Eventually this infix operator should be modified, we wanted ::: but somehow
    this is not possible ...
*)
val (/::/) : 'a t -> 'a t -> 'a t

(** {2 Normalization functions} *)

(**
   @return [(head, tail)] of the input [tile] with respect to the time
*)
val headTail : 'a t -> 'a t * 'a t

(** {2 Testing functions} *) 

(**
   @return the tile containing all events in the [event list] in sequence
*)
val fromList : ('a event) list -> 'a t
