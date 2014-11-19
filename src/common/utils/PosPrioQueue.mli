(** Module implementing a modifiable priority queue on positions with an indexed binary heap *)

(** priority type *)
type priority = int

(** object type *)
type t

(** empty queue *)
val empty : int -> int -> t

val is_empty : t -> bool

val set_empty : t -> unit

val p_none : priority

val print : t -> unit

(** insert an element with a priority in a queue *)
val push : t -> priority -> Position.t -> unit

(** extract the element with the min priority from a queue *)
val pop : t -> priority * Position.t

(** returns the element with the min priority from a queue *)
val top : t -> priority * Position.t

(** decreases the priority of an element *)
val decrease_priority : t -> priority -> Position.t -> unit

exception Queue_is_empty
