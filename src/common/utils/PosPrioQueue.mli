(** Module implementing a mutable priority queue on Positions.t with an indexed binary heap 
    Assume elements unicity, do not add several times the same position
    @author dbusatto*)


(** raised when trying to pop an empty queue*)
exception Queue_is_empty

(** priority type *)
type priority = int

(** priority +infinity *)
val p_none : priority

(** queue type *)
type t

(** [empty w h] returns an empty queue on positions between (0,0) and (w-1,h-1)*)
val empty : int -> int -> t

(** Test if a queue is empty *)
val is_empty : t -> bool

(** {6 Operators} *)

(** Make a queue empty, not reversible (the inernal index isn't emptied, it is indeed not necessary for [is_empty] to return true). Intended to quickly end an algorithm waiting for the queue to be empty.

  Do NOT add positions after calling this method.
  Run faster than [while not (is_empty q) do let _ = pop q in () done] *)
val set_empty : t -> unit

(** Insert a position with a priority in a queue *)
val push : t -> priority -> Position.t -> unit

(** Extract one of the elements with the minimal priority from a queue *)
val pop : t -> priority * Position.t

(** Return one of the elements with the minimal priority from a queue *)
val top : t -> priority * Position.t

(** Decrease the priority of an element *)
val decrease_priority : t -> priority -> Position.t -> unit

(** for debug puposes *)
val print : t -> unit

