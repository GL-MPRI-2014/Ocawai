(* Type Checker *)

open ScriptTypes

(* For debug *)
module CheckerLog = Log.Make (struct let section = "Type Checker" end)
open CheckerLog

(* Associate every variable/function name to its type *)
let assignment = Hashtbl.create 97

exception Unbound_variable of string * location
exception Unbound_function of string * location

let rec deref (t:term_type) =
  match !t with
  | `Pointer v -> deref v
  | v -> v


(* For the alphae *)
(* Returns a fresh type with _'a instead of 'a *)
let underscore_alpha ftype =
  let alpha_env = Hashtbl.create 13 in
  let get_alpha i =
    if Hashtbl.mem alpha_env i then Hashtbl.find alpha_env i
    else begin
      let t = ref `None in
      Hashtbl.add alpha_env i t ;
      t
    end
  in
  let rec aux t =
    match (deref t) with
    | `Alpha_tc i    -> get_alpha i
    | `Fun_tc (a,b)  -> ref (`Fun_tc (aux a, aux b))
    | `List_tc a     -> ref (`List_tc (aux a))
    | `Array_tc a    -> ref (`Array_tc (aux a))
    | `Pair_tc (a,b) -> ref (`Pair_tc (aux a, aux b))
    | _              -> t
  in aux ftype

(* Translates a type to a string *)
let type_to_string t =
  let rec aux parenthesis t =
    match (deref t) with
    | `Int_tc        -> "int"
    | `Unit_tc       -> "unit"
    | `String_tc     -> "string"
    | `Bool_tc       -> "bool"
    | `Soldier_tc    -> "soldier"
    | `Map_tc        -> "map"
    | `Player_tc     -> "player"
    | `Building_tc   -> "building"
    | `Alpha_tc i    -> "'a" ^ (string_of_int i)
    | `List_tc v     -> (aux true v) ^ " list"
    | `Array_tc v    -> (aux true v) ^ " array"
    | `Fun_tc (a,b)  ->
        let s = (aux true a) ^ " -> " ^ (aux false b) in
        if parenthesis then
          "(" ^ s ^ ")"
        else
          s
    | `Pair_tc (a,b) -> "(" ^ (aux true a) ^ " * " ^ (aux true b) ^ ")"
    | `Pointer t     -> assert false
    | `None          -> "any_type"
    in
  aux false t


exception Unification_failure

let rec unify (t1:term_type) (t2:term_type) =
  debugf
    "[unifying] types\t%s\tand\t%s"
    (type_to_string t1) (type_to_string t2) ;
  if t1 <> t2 then
  match (deref t1, deref t2) with
  | `Alpha_tc i, _ -> (*unify (get_alpha i) t2*) assert false
  | _, `Alpha_tc i -> (*unify t1 (get_alpha i)*) assert false
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
exception Build_return of term_type * location
exception Attack_return of term_type * location
exception Init_return of term_type * location
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

  | GlobSeq ((v,k),l) ->
      debug (lazy "global sequence");
      let vt = val_type v in
      (try unify vt (ref `Unit_tc)
      with Unification_failure -> raise (Not_unit_seq (vt,l)));
      check_prog k

  | Empty -> debug (lazy "empty prog")

and check_decl = function

  | Vardecl ((s,v),l) ->
      debugf "var decleration %s" s;
      Hashtbl.add assignment s (val_type v)

  | Varset ((s,v),l) ->
      debugf "var set %s" s;
      begin
        let st =
          try Hashtbl.find assignment s
          with Not_found -> raise (Unbound_variable (s,l))
        and vt = val_type v in
        try unify st vt
        with Unification_failure -> raise (Wrong_type_set (s,st,vt,l))
      end

  | Fundecl ((s,sl,sqt),l) ->
      debug (lazy "function declaration");
      (* First, for each variable, we associate a type *)
      List.iter (fun s -> Hashtbl.add assignment s (ref `None)) sl ;
      let tl = List.map (fun s -> Hashtbl.find assignment s) sl in
      let return_type = ref `None in
      (* We deduce the function type *)
      (* TODO: Infer polymorphism *)
      List.fold_right (fun a b -> ref (`Fun_tc (a,b))) tl return_type |>
      Hashtbl.add assignment s ;
      (* We precise these types by checking the sequence *)
      unify return_type (seq_type sqt) ;
      (* Out of this scope, the variables are no more *)
      List.iter (fun s -> Hashtbl.remove assignment s) sl ;
      (* Debug *)
      debugf "declared function %s of type %s"
        s (type_to_string (Hashtbl.find assignment s))

and check_procedure = function

  | Move ((sl,st),l) ->
      debug (lazy "move");
      let t = seq_type st in
      begin
        try unify t (ref (`List_tc (ref (`Pair_tc (ref `Int_tc, ref `Int_tc)))))
        with Unification_failure -> raise (Move_return (t,l))
      end

  | Attack ((sl,st),l) ->
      debug (lazy "attack");
      let t = seq_type st in
      begin
        try unify t (ref `Soldier_tc)
        with Unification_failure -> raise (Attack_return (t,l))
      end

  | Main (st,l) ->
      debug (lazy "main");
      let t = seq_type st in
      begin
        try unify t (ref `Soldier_tc)
        with Unification_failure -> raise (Main_return (t,l))
      end

  | Build ((sl,st),l) ->
      debug (lazy "attack");
      let t = seq_type st in
      begin
        try unify t (ref `String_tc)
        with Unification_failure -> raise (Build_return (t,l))
      end

  | Init (st,l) ->
      debug (lazy "init");
      let t = seq_type st in
      begin
        try unify t (ref `Unit_tc)
        with Unification_failure -> raise (Init_return (t,l))
      end

and val_type = function

  | Int (_,l)    -> debug (lazy "int"); ref `Int_tc
  | Unit (l)     -> debug (lazy "unit"); ref `Unit_tc
  | String (_,l) -> debug (lazy "string"); ref `String_tc
  | Bool (_,l)   -> debug (lazy "bool"); ref `Bool_tc
  | List (vl,l)  ->
      debug (lazy "list");
      let alpha = ref `None in
      List.iter
        (fun v ->
          let vt = val_type v in
          try unify vt alpha
          with Unification_failure -> raise (Hetero_list (alpha,vt,l))
        )
        vl ;
      ref (`List_tc alpha)
  | Array (va,l) ->
      debug (lazy "array");
      let alpha = ref `None in
      Array.iter
        (fun v ->
          let vt = val_type v in
          try unify vt alpha
          with Unification_failure -> raise (Hetero_array (alpha,vt,l))
        ) va ;
      ref (`Array_tc alpha)
  | Var (s,l)    ->
      debugf "var %s" s;
      (try
        let t = underscore_alpha (Hashtbl.find assignment s) in
        debugf "type %s" (type_to_string t) ;
        t
      with Not_found -> raise (Unbound_variable (s,l)))
  | App ((s,vl),l) ->
      debugf "application %s" s;
      begin
        let ftype' =
          try Hashtbl.find assignment s
          with Not_found -> raise (Unbound_function (s,l))
        in
        debugf "%s is of type %s" s (type_to_string ftype');
        let ftype = underscore_alpha ftype' in
        debugf "deduced type %s" (type_to_string ftype);
        debug (lazy "computing argument types");
        let argst = List.map val_type vl in
        debug (lazy "return type");
        let return_type =
          try unify_func ftype argst
          with Unification_failure -> raise (Apply_args (s,ftype',argst,l))
        in
        debug (lazy "end application") ;
        return_type
      end
  | Ifte ((v,s1,s2),l) ->
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
      begin
        try unify st1 st2
        with Unification_failure -> raise (Different_type_else (st1,st2,l))
      end;
      debug (lazy "end if");
      st1
  | Pair ((v1,v2),l) ->
      debug (lazy "pair");
      ref (`Pair_tc (val_type v1, val_type v2))

and seq_type = function

  | SeqDecl ((d,k),l) ->
      debug (lazy "sequence declaration");
      check_decl d ;
      seq_type k

  | SeqVar ((v,SeqEnd),l) ->
      debug (lazy "sequence value (terminating)");
      val_type v

  | SeqVar ((v,k),l) ->
      debug (lazy "sequence value (with continuation)");
      let t = val_type v in
      begin
        try unify t (ref `Unit_tc)
        with Unification_failure -> raise (Not_unit_seq (t,l))
      end ;
      seq_type k

  | SeqEnd -> debug (lazy "sequence end"); ref `Unit_tc


let printable_location (l,l') =
  Lexing.(Printf.sprintf
    "Error in %s, from line %d characters %d-%d to line %d characters %d-%d: "
    l.pos_fname
    l.pos_lnum
    l.pos_bol
    l.pos_bol
    l'.pos_lnum
    l'.pos_bol
    l'.pos_bol)


exception Type_checking_failure

let rec v_to_tctype = function
  |`Int_t         -> ref `Int_tc
  |`Unit_t        -> ref `Unit_tc
  |`String_t      -> ref `String_tc
  |`Bool_t        -> ref `Bool_tc
  |`Soldier_t     -> ref `Soldier_tc
  |`Map_t         -> ref `Map_tc
  |`Player_t      -> ref `Player_tc
  |`Building_t    -> ref `Building_tc
  |`Alpha_t (i)   -> ref (`Alpha_tc (i))
  |`List_t  (v)   -> ref (`List_tc (v_to_tctype v))
  |`Array_t (v)   -> ref (`Array_tc (v_to_tctype v))
  |`Fun_t   (v,v')-> ref (`Fun_tc (v_to_tctype v, v_to_tctype v'))
  |`Pair_t  (v,v')-> ref (`Pair_tc (v_to_tctype v, v_to_tctype v'))

let type_check prog types =
  List.iter (fun (s,t) -> Hashtbl.add assignment s (v_to_tctype t)) types;
  let pp = errorf in
  let okay = ref false in
  begin
    try check_prog prog ; okay := true
    with
    | Unification_failure ->
        pp "Couldn't unify (more precisions soon)\n"
    | Unbound_variable (s,l) ->
        pp "%svariable %s is unbound\n" (printable_location l) s
    | Unbound_function (s,l) ->
        pp "%sfunction %s is unbound\n" (printable_location l) s
    | Not_unit_seq (v,l) ->
        pp
          "%sexpected type unit in sequence, got %s\n"
          (printable_location l) (type_to_string v)
    | Wrong_type_set (s,st,vt,l) ->
        pp
          "%svariable %s of type %s cannot be set to type %s\n"
          (printable_location l)
          s
          (type_to_string st)
          (type_to_string vt)
    | Move_return (t,l) ->
        pp
          "%sMove block should return a path of type (int * int) list, received %s\n"
          (printable_location l)
          (type_to_string t)
    | Main_return (t,l) ->
        pp
          "%sMain should return the next unit to be played of type soldier, received %s\n"
          (printable_location l)
          (type_to_string t)
    | Attack_return (t,l) ->
        pp
          "%sAttack should return the unit to be attacked of type soldier, received %s\n"
          (printable_location l)
          (type_to_string t)
    | Init_return (t,l) ->
        pp
          "%sInit return type should be unit, received %s\n"
          (printable_location l)
          (type_to_string t)
    | Hetero_list (a,t,l) ->
        pp
          "%sheterogenous list of type %s list cannot contain element of type %s\n"
          (printable_location l)
          (type_to_string a)
          (type_to_string t)
    | Hetero_array (a,t,l) ->
        pp
        "%sheterogenous array of type %s array cannot contain element of type %s\n"
        (printable_location l)
        (type_to_string a)
        (type_to_string t)
    | Apply_args (s,ft,tl,l) ->
        pp
          "%sfunction %s of type %s cannot be applied to parameters of types "
          (printable_location l)
          s
          (type_to_string ft) ;
        List.iter (fun t -> pp "%s, " (type_to_string t)) tl;
        pp "it doesn't match\n"
    | Not_bool_if (t,l) ->
        pp
          "%sIf statement's condition should be of type bool, received %s\n"
          (printable_location l)
          (type_to_string t)
    | Different_type_else (t1,t2,l) ->
        pp
          "%sElse statement returns a value of type %s when the If statement returned a value of type %s\n"
          (printable_location l)
          (type_to_string t2)
          (type_to_string t1)
  end ;
  if not (!okay) then raise Type_checking_failure



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
  | `Building_t   -> ref `Building_tc

let expose (v:ScriptValues.value_type) s =
  Hashtbl.add assignment s (vt_to_tt v)

let hide s =
  Hashtbl.remove assignment s
