(** Module implementing a priority queue with a binary heap *)

(** priority type *)
type priority = int

(** object type *)
type 'a queue

(** empty queue *)
val empty : 'a queue

(** insert an element with a priority in a queue *)
val push : 'a queue -> priority -> 'a -> 'a queue

(** extract the element with the min priority from a queue *)
val pop : 'a queue -> priority * 'a * 'a queue

(** returns the element with the min priority from a queue *)
val top : 'a queue -> priority * 'a

exception Queue_is_empty
