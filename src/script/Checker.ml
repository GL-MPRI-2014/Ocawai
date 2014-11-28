(* Type Checker *)

open Types

(* Associate every variable name to its type *)
let assignment = Hashtbl.create 97

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


(* Assuming every variable is in assignment *)
let rec check_prog = function

  | Globseq ((d,k),l,t) ->
      check_decl d ;
      unify t (ref `Unit_tc) ;
      check_prog k

  | Procseq ((p,k),l,t) ->
      check_procedure p ;
      unify t (ref `Unit_tc) ;
      check_prog k

  | Empty -> ()

and check_decl = function

  | Vardecl ((s,v),l,t) ->
      unify (Hashtbl.find assignment s) (val_type v) ;
      unify t (ref `Unit_tc)

  | Varset ((s,v),l,t) ->
      unify (Hashtbl.find assignment s) (val_type v) ;
      unify t (ref `Unit_tc)

  | Fundecl ((s,sl,sqt),l,t) ->
      (* TODO *)
      unify t (ref `Unit_tc)

and check_procedure = function

  | Move ((sl,st),l,t) ->
      (* TODO *)
      unify t (ref `Unit_tc)

  | Attack ((sl,st),l,t) ->
      (* TODO *)
      unify t (ref `Unit_tc)

  | Main (st,l,t) ->
      (* TODO *)
      unify t (ref `Unit_tc)

  | Init (st,l,t) ->
      (* TODO *)
      unify t (ref `Unit_tc)

and val_type = function

  | Int (_,l,t) -> t := `Int_tc ; t
  | Unit (l,t) -> t := `Unit_tc ; t
  | _ -> ref `None (* TODO *)
