(**
 * Doubly chained list module used in music generation.
 * implements constant time insert in head AND tail.
 * Based on the TPTM model developed by P. Hudak and
 * D. Janin, code inspired by [Théis]' internship.
**)

type time
type event
type t

(**
 * Delay to sync various tiles
**)
val sync : time -> t

(**
 * Neutral element for the Tiled product
**)
val zero : t

(**
 * Compares t to zero
**)
val isZero : t -> bool

(**
 * Encapsulate an event into a singleton, that is a single element list
**)
val return : event -> t

(**
 * Infix operation for concatenation
 *
 * Eventually this infix operator should be modified, we wanted ::: but somehow
 * this is not possible ...
**)
val (/::/) : t -> t -> t

(**
 * Split the abstract tree to its head and tail with respect to the time
**)
val headTail : t -> t * t

(**
 * For testing purpose
**)
val fromList : event list -> t

(**
 * Idem
**)
val toList : t -> event list

