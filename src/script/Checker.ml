(** Type Checker *)

open Types

let rec deref (t:term_type) =
  match !t with
  | `Pointer v -> deref v
  | v -> v

exception Unification_failure

let unify (t1:term_type) (t2:term_type) =
  match (deref t1, deref t2) with
  | `None, _ -> t1 := `Pointer t2
  | _, `None -> t2 := `Pointer t1
  | t1,t2 -> if t1 <> t2 then raise Unification_failure




(* let rec deref (t : term_type) = match !t with
  | `Unifier t -> deref (!t)
  | _ -> t

(* let bind x t =

let unify t1 t2 =
  match (!t1,!t2) with
  | `None, _ ->  *)

(* let check_type term t =
  true *)

exception Unification_failure

let unify t1 t2 = match (deref t1, deref t2) with
  | `None, v -> t1 := v
  | v, `None -> t2 := v
  (* | `List_t t1, `List_t t2 -> (*unify t1 t2*) if t1 <> t2 raise Unification_failure
  | `Array_t t1, `Array_t t2 -> (*unify t1 t2*) *)
  | t1, t2 -> if t1 <> t2 then raise Unification_failure

let var_type s =
  `Int *)

(* let check_prog = function
  | Globseq ((d,k),t) ->
      check_decl d ;
      assert (check_type t `Unit) ;
      check_prog k
  | Procseq ((p,k),t) ->
      check_procedure p ;
      assert (check_type t `Unit) ;
      check_prog k
  | Empty -> ()

and check_decl = function
  | Vardecl ((s,v),t) -> *)
      (* let vart = var_type s in
      let valt = value_type v in
      unify vart valt *)
