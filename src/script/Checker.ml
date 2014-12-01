(* Type Checker *)

open ScriptTypes

(* Associate every variable/function name to its type *)
let assignment = Hashtbl.create 97

exception Unbound_variable of string * location
exception Unbound_function of string * location

let rec deref (t:term_type) =
  match !t with
  | `Pointer v -> deref v
  | v -> v


(* TODO: make the error more precise *)
exception Unification_failure

let rec unify (t1:term_type) (t2:term_type) =
  match (deref t1, deref t2) with
  | `Alpha_tc, _ -> ()
  | _, `Alpha_tc -> ()
  | `None, _     -> t1 := `Pointer t2
  | _, `None     -> t2 := `Pointer t1
  | `List_tc t1, `List_tc t2   -> unify t1 t2
  | `Array_tc t1, `Array_tc t2 -> unify t1 t2
  | `Fun_tc (t1,t1'), `Fun_tc (t2,t2')   -> unify t1 t2 ; unify t1' t2'
  | `Pair_tc (t1,t1'), `Pair_tc (t2,t2') -> unify t1 t2 ; unify t1' t2'
  | t1,t2 -> if t1 <> t2 then raise Unification_failure


(* Unifies a function with a list of arguments types *)
(* Returns the return type of the function *)
(* It can still be a function if applied to too few arguments *)
let rec unify_func (ftype : term_type) = function
  | []      -> ftype
  | e :: r  ->
      (match (deref ftype) with
        | `Fun_tc (a,b) ->
            unify e a ;
            unify_func b r
        | _ ->
            if r <> [] then raise Unification_failure ;
            unify e ftype ;
            ftype
      )


let rec check_prog = function

  | GlobDecl ((d,k),l) ->
      check_decl d ;
      check_prog k

  | GlobProc ((p,k),l) ->
      check_procedure p ;
      check_prog k

  (* It seems that all these are unit *)
  (* | GlobSeq ((v,Empty),l,t) ->
      unify t (val_type v) ;
      unify t (ref `Unit_tc) *)

  | GlobSeq ((v,k),l,t) ->
      unify t (val_type v) ;
      unify t (ref `Unit_tc) ;
      check_prog k

  | Empty -> ()

and check_decl = function

  | Vardecl ((s,v),l) ->
      Hashtbl.add assignment s (val_type v)

  | Varset ((s,v),l) ->
      (try unify (Hashtbl.find assignment s) (val_type v)
      with Not_found -> raise (Unbound_variable (s,l)))

  | Fundecl ((s,sl,sqt),l) ->
      (* First, for each variable, we associate a type *)
      List.iter (fun s -> Hashtbl.add assignment s (ref `None)) sl ;
      let tl = List.map (fun s -> Hashtbl.find assignment s) sl in
      let return_type = ref `None in
      (* We deduce the function type *)
      List.fold_right (fun a b -> ref (`Fun_tc (a,b))) tl return_type |>
      Hashtbl.add assignment s ;
      (* We precise these types by checking the sequence *)
      unify return_type (seq_type sqt) ;
      (* Out of this scope, the variables are no more *)
      List.iter (fun s -> Hashtbl.remove assignment s) sl

and check_procedure = function

  | Move ((sl,st),l,t) ->
      Hashtbl.add assignment "selected_unit" (ref `Soldier_tc) ;
      unify t (seq_type st) ;
      unify t (ref (`List_tc (ref (`Pair_tc (ref `Int_tc, ref `Int_tc)))))

  | Attack ((sl,st),l,t) ->
      unify t (seq_type st) ;
      unify t (ref `Soldier_tc)

  | Main (st,l,t) ->
      unify t (seq_type st) ;
      unify t (ref `Soldier_tc)

  | Init (st,l,t) ->
      unify t (seq_type st) ;
      unify t (ref `Unit_tc)

and val_type = function

  | Int (_,l,t)    -> unify t (ref `Int_tc)    ; t
  | Unit (l,t)     -> unify t (ref `Unit_tc)   ; t
  | String (_,l,t) -> unify t (ref `String_tc) ; t
  | Bool (_,l,t)   -> unify t (ref `Bool_tc)   ; t
  | List (vl,l,t)  ->
      let alpha = ref `None in
      List.iter (fun v -> unify (val_type v) alpha) vl ;
      unify t (ref (`List_tc alpha)) ; t
  | Array (va,l,t) ->
      let alpha = ref `None in
      Array.iter (fun v -> unify (val_type v) alpha) va ;
      unify t (ref (`Array_tc alpha)) ; t
  | Var (s,l,t)    ->
      (try unify (Hashtbl.find assignment s) t ; t
      with Not_found -> raise (Unbound_variable (s,l)))
  | App ((s,vl),l,t) ->
      (try let rt = unify_func
                (Hashtbl.find assignment s)
                (List.map val_type vl)
        in
        unify t rt ; t
      with Not_found -> raise (Unbound_function (s,l)))
  | Ifte ((v,s1,s2),l,t) ->
      unify (val_type v) (ref `Bool_tc) ;
      unify t (seq_type s1) ;
      unify t (seq_type s2) ;
      t
  | Pair ((v1,v2),l,t) ->
      unify t (ref (`Pair_tc (val_type v1, val_type v2))) ; t

and seq_type = function

  | SeqDecl ((d,k),l,t) ->
      check_decl d ;
      unify t (ref `Unit_tc) ;
      seq_type k

  | SeqVar ((v,SeqEnd),l,t) ->
      unify t (val_type v) ;
      t

  | SeqVar ((v,k),l,t) ->
      unify t (val_type v) ;
      unify t (ref `Unit_tc) ;
      seq_type k

  | SeqEnd -> ref `None


let print_location (l,l') =
  Lexing.(Printf.printf
    "Error in %s, from line %d characters %d-%d to line %d characters %d-%d: "
    l.pos_fname
    l.pos_lnum
    l.pos_bol
    l.pos_bol
    l'.pos_lnum
    l'.pos_bol
    l'.pos_bol)

let type_check prog =
  (* TODO: find a better place? *)
  Hashtbl.add assignment "self" (ref `Player_tc) ;
  Hashtbl.add assignment "players" (ref (`List_tc (ref `Player_tc))) ;
  Hashtbl.add assignment "map" (ref `Map_tc) ;
  Hashtbl.add assignment "selected_unit" (ref `Soldier_tc) ;
  try check_prog prog
  with
  | Unification_failure ->
      Printf.printf "Error: Couldn't unify (more precisions soon)\n"
  | Unbound_variable (s,l) ->
      print_location l ;
      Printf.printf "variable %s is unbound\n" s
  | Unbound_function (s,l) ->
      print_location l ;
      Printf.printf "function %s is unbound\n" s


(* Translates a ScriptValues.value_type to a term_type *)
let rec vt_to_tt = function
  | `Int_t        -> ref `Int_tc
  | `Unit_t       -> ref `Unit_tc
  | `String_t     -> ref `String_tc
  | `Bool_t       -> ref `Bool_tc
  | `Soldier_t    -> ref `Soldier_tc
  | `Map_t        -> ref `Map_tc
  | `Player_t     -> ref `Player_tc
  | `Alpha_t i    -> ref `Alpha_tc (*TODO*)
  | `List_t v     -> ref (`List_tc (vt_to_tt v))
  | `Array_t v    -> ref (`Array_tc (vt_to_tt v))
  | `Fun_t (a,b)  -> ref (`Fun_tc (vt_to_tt a, vt_to_tt b))
  | `Pair_t (a,b) -> ref (`Pair_tc (vt_to_tt a, vt_to_tt b))

let expose (v:ScriptValues.value_type) s =
  Hashtbl.add assignment s (vt_to_tt v)

let hide s =
  Hashtbl.remove assignment s
