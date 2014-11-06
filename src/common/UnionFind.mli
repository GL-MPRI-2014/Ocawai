(** Note : the operations are not yet optimized *)

(** Type of objects *)
type 'a node

(** Make a singleton containing a data *)
val make_sing : 'a -> 'a node

(** Join two subsets represented by their two representatives *)
val union : 'a node -> 'a node -> unit

(** General union : join two subsets *)
val union_gen : 'a node -> 'a node -> unit

(** Find the representative *)
val find : 'a node -> 'a node

(** [get_data x] returns the list of data of the subset
  represented by its representative x*)
val get_data : 'a node -> 'a list

(** General get_data : [get_data_gen x] returns the list of data of
  the subset containing x*)
val get_data_gen : 'a node -> 'a list

