(** Module implementing a mutable priority queue on Positions.t with an indexed binary heap 
    Assume elements unicity, do not add several times the same position*)

(** priority type *)
type priority = int

(** object type *)
type t

(** empty queue *)
val empty : int -> int -> t

(** test if a queue is empty *)
val is_empty : t -> bool

(** make a queue empty, not reversible
  (do NOT add positions after calling this method) *)
val set_empty : t -> unit

(** priority +infinity *)
val p_none : priority

(** for debug puposes *)
val print : t -> unit

(** insert an position with a priority in a queue *)
val push : t -> priority -> Position.t -> unit

(** extract an element with the minimal priority from a queue *)
val pop : t -> priority * Position.t

(** returns an element with the minimal priority from a queue *)
val top : t -> priority * Position.t

(** decreases the priority of an element *)
val decrease_priority : t -> priority -> Position.t -> unit

exception Queue_is_empty
