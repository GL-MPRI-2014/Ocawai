(** Type checker *)

(* Will probably be hidden later, just for typing *)
(* I would also like not to have `Pointer in the output type, but the
   typechecker doesn't believe me... *)
val deref : Types.term_type -> [Types.static | `None | `Pointer of Types.term_type]

exception Unification_failure

val unify : Types.term_type -> Types.term_type -> unit
