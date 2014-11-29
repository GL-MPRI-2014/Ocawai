(** Type checker *)

(* Will probably be hidden later, just for typing *)
(* I would also like not to have `Pointer in the output type, but the
   typechecker doesn't believe me... *)
val deref : ScriptTypes.term_type -> ScriptTypes.static

exception Unification_failure

val unify : ScriptTypes.term_type -> ScriptTypes.term_type -> unit