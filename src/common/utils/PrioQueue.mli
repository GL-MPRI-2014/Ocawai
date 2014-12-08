(** Module implementing a priority queue with a binary heap 
    @author dbusatto*)


(** priority type *)
type priority = int

(** raised when trying to pop an empty queue*)
exception Queue_is_empty

(** queue type *)
type 'a queue

(** empty queue *)
val empty : 'a queue

(** Insert an element with a priority in a queue *)
val push : 'a queue -> priority -> 'a -> 'a queue

(** Extract the element with the min priority from a queue *)
val pop : 'a queue -> priority * 'a * 'a queue

(** Return the element with the min priority from a queue *)
val top : 'a queue -> priority * 'a

