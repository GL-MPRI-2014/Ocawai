(* Type Checker *)

open ScriptTypes

(* For debug *)
module CheckerLog = Log.Make (struct let section = "Type Checker" end)
open CheckerLog

(* Associate every variable/function name to its type *)
let assignment = Hashtbl.create 97

(* For the alphae *)
(* Allows to unify them in a given context *)
let alpha_env = Hashtbl.create 97
let get_alpha i =
  if Hashtbl.mem alpha_env i then Hashtbl.find alpha_env i
  else begin
    let t = ref `None in
    Hashtbl.add alpha_env i t ;
    t
  end

exception Unbound_variable of string * location
exception Unbound_function of string * location

let rec deref (t:term_type) =
  match !t with
  | `Pointer v -> deref v
  | v -> v


(* Translates a type to a string *)
let rec type_to_string t =
  match (deref t) with
  | `Int_tc        -> "int"
  | `Unit_tc       -> "unit"
  | `String_tc     -> "string"
  | `Bool_tc       -> "bool"
  | `Soldier_tc    -> "soldier"
  | `Map_tc        -> "map"
  | `Player_tc     -> "player"
  | `Alpha_tc i    -> "alpha_" ^ (string_of_int i)
  | `List_tc v     -> (type_to_string v) ^ " list"
  | `Array_tc v    -> (type_to_string v) ^ " array"
  | `Fun_tc (a,b)  -> (type_to_string a) ^ " -> (" ^ (type_to_string b) ^ ")"
  | `Pair_tc (a,b) -> "(" ^ (type_to_string a) ^ " * " ^ (type_to_string b) ^ ")"
  | `Pointer t     -> assert false
  | `None          -> "any_type"


exception Unification_failure

let rec unify (t1:term_type) (t2:term_type) =
  debugf
    "[unifying] types\t(%s)\tand\t(%s)"
    (type_to_string t1) (type_to_string t2) ;
  if t1 <> t2 then
  match (deref t1, deref t2) with
  | `Alpha_tc i, _ -> unify (get_alpha i) t2
  | _, `Alpha_tc i -> unify t1 (get_alpha i)
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


(* Exceptions regarding errors *)
exception Not_unit_seq of term_type * location
exception Wrong_type_set of string * term_type * term_type * location
exception Move_return of term_type * location
exception Main_return of term_type * location
(* TODO Add Init and Attack errors *)
exception Hetero_list of term_type * term_type * location
exception Hetero_array of term_type * term_type * location
exception Apply_args of string * term_type * (term_type list) * location
exception Not_bool_if of term_type * location
exception Different_type_else of term_type * term_type * location


let rec check_prog = function

  | GlobDecl ((d,k),l) ->
      debug (lazy "global decleration");
      check_decl d ;
      check_prog k

  | GlobProc ((p,k),l) ->
      debug (lazy "global procedure");
      check_procedure p ;
      check_prog k

  | GlobSeq ((v,k),l,t) ->
      debug (lazy "global sequence");
      let vt = val_type v in
      unify t vt ;
      (try unify t (ref `Unit_tc)
      with Unification_failure -> raise (Not_unit_seq (vt,l)));
      check_prog k

  | Empty -> debug (lazy "empty prog")

and check_decl = function

  | Vardecl ((s,v),l) ->
      debug (lazy "var decleration");
      Hashtbl.add assignment s (val_type v)

  | Varset ((s,v),l) ->
      debug (lazy "var set");
      begin
        let st =
          try Hashtbl.find assignment s
          with Not_found -> raise (Unbound_variable (s,l))
        and vt = val_type v in
        try unify st vt
        with Unification_failure -> raise (Wrong_type_set (s,st,vt,l))
      end

  | Fundecl ((s,sl,sqt),l) ->
      debug (lazy "function decleration");
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
      debug (lazy "move");
      Hashtbl.add assignment "selected_unit" (ref `Soldier_tc) ;
      unify t (seq_type st) ;
      begin
        try unify t (ref (`List_tc (ref (`Pair_tc (ref `Int_tc, ref `Int_tc)))))
        with Unification_failure -> raise (Move_return (t,l))
      end

  | Attack ((sl,st),l,t) ->
      debug (lazy "attack");
      unify t (seq_type st) ;
      unify t (ref `Soldier_tc)

  | Main (st,l,t) ->
      debug (lazy "main");
      unify t (seq_type st) ;
      begin
        try unify t (ref `Soldier_tc)
        with Unification_failure -> raise (Main_return (t,l))
      end

  | Init (st,l,t) ->
      debug (lazy "init");
      unify t (seq_type st) ;
      unify t (ref `Unit_tc)

and val_type = function

  | Int (_,l,t)    -> debug (lazy "int"); unify t (ref `Int_tc)    ; t
  | Unit (l,t)     -> debug (lazy "unit"); unify t (ref `Unit_tc)   ; t
  | String (_,l,t) -> debug (lazy "string"); unify t (ref `String_tc) ; t
  | Bool (_,l,t)   -> debug (lazy "bool"); unify t (ref `Bool_tc)   ; t
  | List (vl,l,t)  ->
      debug (lazy "list");
      let alpha = ref `None in
      List.iter
        (fun v ->
          let vt = val_type v in
          try unify vt alpha
          with Unification_failure -> raise (Hetero_list (alpha,vt,l))
        )
        vl ;
      unify t (ref (`List_tc alpha)) ; t
  | Array (va,l,t) ->
      debug (lazy "array");
      let alpha = ref `None in
      Array.iter
        (fun v ->
          let vt = val_type v in
          try unify vt alpha
          with Unification_failure -> raise (Hetero_array (alpha,vt,l))
        ) va ;
      unify t (ref (`Array_tc alpha)) ; t
  | Var (s,l,t)    ->
      debug (lazy "var");
      (try unify (Hashtbl.find assignment s) t ; t
      with Not_found -> raise (Unbound_variable (s,l)))
  | App ((s,vl),l,t) ->
      debug (lazy "application");
      begin
        let ftype =
          try Hashtbl.find assignment s
          with Not_found -> raise (Unbound_function (s,l))
        and argst = List.map val_type vl in
        debug (lazy "return type");
        let return_type =
          try unify_func ftype argst
          with Unification_failure -> raise (Apply_args (s,ftype,argst,l))
        in
        (* We don't want the alpha_i to remain bound *)
        (* TODO Check if it works for higher order *)
        Hashtbl.clear alpha_env ;
        unify t return_type ; debug (lazy "end application") ; t
      end
  | Ifte ((v,s1,s2),l,t) ->
      debug (lazy "if condition");
      let vt = val_type v in
      begin
        try unify vt (ref `Bool_tc)
        with Unification_failure -> raise (Not_bool_if (vt,l))
      end;
      debug (lazy "if statement");
      let st1 = seq_type s1 in
      debugf "if returns %s" (type_to_string st1);
      debug (lazy "else statement");
      let st2 = seq_type s2 in
      debugf "else returns %s" (type_to_string st1);
      unify t st1 ;
      begin
        try unify t st2
        with Unification_failure -> raise (Different_type_else (st1,st2,l))
      end;
      debug (lazy "end if");
      t
  | Pair ((v1,v2),l,t) ->
      debug (lazy "pair");
      unify t (ref (`Pair_tc (val_type v1, val_type v2))) ; t

and seq_type = function

  | SeqDecl ((d,k),l,t) ->
      debug (lazy "sequence declaration");
      check_decl d ;
      unify t (ref `Unit_tc) ;
      seq_type k

  | SeqVar ((v,SeqEnd),l,t) ->
      debug (lazy "sequence value (terminating)");
      unify t (val_type v) ;
      t

  | SeqVar ((v,k),l,t) ->
      debug (lazy "sequence value (with continuation)");
      unify t (val_type v) ;
      begin
        try unify t (ref `Unit_tc)
        with Unification_failure -> raise (Not_unit_seq (t,l))
      end ;
      seq_type k

  | SeqEnd -> debug (lazy "sequence end"); ref `Unit_tc


let print_location (l,l') =
  Lexing.(errorf
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
  (* Simplifying a bit *)
  let pp = errorf in
  try check_prog prog
  with
  | Unification_failure ->
      pp "Error: Couldn't unify (more precisions soon)\n"
  | Unbound_variable (s,l) ->
      print_location l ;
      pp "variable %s is unbound\n" s
  | Unbound_function (s,l) ->
      print_location l ;
      pp "function %s is unbound\n" s
  | Not_unit_seq (v,l) ->
      print_location l ;
      pp "expected type unit in sequence, got %s\n" (type_to_string v)
  | Wrong_type_set (s,st,vt,l) ->
      print_location l ;
      pp
        "variable %s of type %s cannot be set to type %s\n"
        s
        (type_to_string st)
        (type_to_string vt)
  | Move_return (t,l) ->
      print_location l ;
      pp
        "Move block should return a path of type (int * int) list, received %s\n"
        (type_to_string t)
  | Main_return (t,l) ->
      print_location l ;
      pp "Main should return the next unit to be played of type soldier, received %s\n" (type_to_string t)
  | Hetero_list (a,t,l) ->
      print_location l ;
      pp
        "heterogenous list of type %s list cannot contain element of type %s\n"
        (type_to_string a)
        (type_to_string t)
  | Hetero_array (a,t,l) ->
      print_location l ;
      pp
      "heterogenous array of type %s array cannot contain element of type %s\n"
      (type_to_string a)
      (type_to_string t)
  | Apply_args (s,ft,tl,l) ->
      print_location l ;
      pp
        "function %s of type %s cannot be applied to parameters of types "
        s
        (type_to_string ft) ;
      List.iter (fun t -> pp "%s, " (type_to_string t)) tl;
      pp "it doesn't match\n"
  | Not_bool_if (t,l) ->
      print_location l ;
      pp
        "If statement's condition should be of type bool, received %s\n"
        (type_to_string t)
  | Different_type_else (t1,t2,l) ->
      print_location l ;
      pp
        "Else statement returns a value of type %s when the If statement returned a value of type %s\n"
        (type_to_string t2)
        (type_to_string t1)



(* Translates a ScriptValues.value_type to a term_type *)
let rec vt_to_tt = function
  | `Int_t        -> ref `Int_tc
  | `Unit_t       -> ref `Unit_tc
  | `String_t     -> ref `String_tc
  | `Bool_t       -> ref `Bool_tc
  | `Soldier_t    -> ref `Soldier_tc
  | `Map_t        -> ref `Map_tc
  | `Player_t     -> ref `Player_tc
  | `Alpha_t i    -> ref (`Alpha_tc i)
  | `List_t v     -> ref (`List_tc (vt_to_tt v))
  | `Array_t v    -> ref (`Array_tc (vt_to_tt v))
  | `Fun_t (a,b)  -> ref (`Fun_tc (vt_to_tt a, vt_to_tt b))
  | `Pair_t (a,b) -> ref (`Pair_tc (vt_to_tt a, vt_to_tt b))

let expose (v:ScriptValues.value_type) s =
  Hashtbl.add assignment s (vt_to_tt v)

let hide s =
  Hashtbl.remove assignment s
