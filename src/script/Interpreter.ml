open ScriptTypes
open ScriptValues

type var_environment = (string * value ref) list


exception Unbound_variable of string

exception Unbound_function of string

exception Entry_point_missing of string


class entrypoints = object(self)

  val mutable main : seq_type option = None

  val mutable init : seq_type option = None

  val mutable moves : (string, seq_type) Hashtbl.t = Hashtbl.create 13

  val mutable attacks : (string, seq_type) Hashtbl.t = Hashtbl.create 13

  method add_main t = main <- Some(t)

  method add_init t = main <- Some(t)

  method add_move (sl, t) = 
    List.iter (fun s -> Hashtbl.add moves s t) sl

  method add_attack (sl, t) = 
    List.iter (fun s -> Hashtbl.add attacks s t) sl

  method main = 
    match main with
    |None -> raise (Entry_point_missing "Missing main")
    |Some(m) -> m

  method init = 
    match init with
    |None -> raise (Entry_point_missing "Missing init")
    |Some(m) -> m

  method move s = 
    try Hashtbl.find moves s 
    with |Not_found -> begin
      try Hashtbl.find moves "default"
      with |Not_found -> raise (Entry_point_missing 
        ("No move method for " ^ s ^ " or default."))
    end

  method attack s = 
    try Hashtbl.find attacks s 
    with |Not_found -> begin
      try Hashtbl.find attacks "default"
      with |Not_found -> raise (Entry_point_missing 
        ("No attack method for " ^ s ^ " or default."))
    end

end

type script = var_environment * entrypoints


let get_value s (env : var_environment) = 
  let rec aux = function
    |[] -> raise (Unbound_variable s)
    |(n,v)::q -> if n = s then !v else aux q
  in aux env

let set_value s v (env : var_environment) = 
  let rec aux = function
    |[] -> raise (Unbound_variable s)
    |(n,v')::q  -> 
      if n = s then v' := v
      else aux q
  in aux env; env

let new_value s v (env : var_environment) = 
  (s, ref v) :: env

let get_global_value s (env : var_environment) = 
  try get_value s env 
  with |Unbound_variable(_) -> begin
    try ScriptValues.value_of s 
    with |ScriptValues.Script_value_not_found -> 
      raise (Unbound_variable s)
  end

let rec eval_proc entries = function
  |Move (p,_,_) -> entries#add_move p
  |Attack (p,_,_) -> entries#add_attack p
  |Main(p,_,_) -> entries#add_main p
  |Init(p,_,_) -> entries#add_init p

and create_lambda env args seq = 
  match args with
  |[] -> `Fun(fun _ -> eval_seq env seq)
  |[t] -> `Fun(fun a -> eval_seq (new_value t a env) seq)
  |t::q -> `Fun(fun a -> create_lambda (new_value t a env) q seq)

and apply_f env f args = 
  match (f,args) with
  |(`Fun(f), h::t) -> apply_f env (f h) t
  |( v     , [] ) -> v
  | _ -> assert false

and eval_value env = function
  |Int(i,_,_)    -> `Int(i)
  |Unit(_,_)     -> `Unit
  |String(s,_,_) -> `String(s)
  |Bool(b,_,_)   -> `Bool(b)
  |List(l,_,_)   -> `List(List.map (eval_value env) l)
  |Array(a,_,_)  -> `Array(Array.map (eval_value env) a)
  |Var(s,_,_)    -> get_global_value s env
  |App((f,args),_,_) -> apply_f env (get_global_value f env) 
    (List.map (eval_value env) args)
  |Ifte((v,seq1,seq2),_,_) -> begin
    match eval_value env v with
    |`Bool(b) -> if b then eval_seq env seq1 else eval_seq env seq2
    | _ -> assert false
  end
  |Pair((v1,v2),_,_) -> `Pair(eval_value env v1, eval_value env v2)

and eval_decl env = function
  |Vardecl((s,v),_,_) -> 
      let v' = eval_value env v in 
      new_value s v' env
  |Varset ((s,v),_,_) -> 
      let v' = eval_value env v in
      set_value s v' env
  |Fundecl ((s,args,seq),_,_) -> 
      let env' = new_value s (`Fun(fun _ -> assert false)) env in
      set_value s (create_lambda env' args seq) env'

and eval_seq env = function
  |SeqDecl((decl, seq),_,_) -> 
      let env' = eval_decl env decl in 
      eval_seq env' seq
  |SeqVar((v, SeqEnd),_,_) ->
      eval_value env v
  |SeqVar((v, seq),_,_) ->
      ignore(eval_value env v);
      eval_seq env seq
  |SeqEnd -> `Unit

and eval_prog env entries = function
  |GlobDecl ((decl, prog), _, _) -> 
      let env' = eval_decl env decl in
      eval_prog env' entries prog
  |GlobProc ((proc, prog), _, _) -> 
      eval_proc entries proc;
      eval_prog env entries prog
  |GlobSeq  ((v, prog),_,_) ->
      ignore (eval_value env v);
      eval_prog env entries prog
  |Empty -> env


let new_script prog = 
  let ep = new entrypoints in
  let env = eval_prog [] ep prog in
  (env, ep)

let init_script (env, ep) = 
  eval_seq env ep#init
  |> ignore

let pair_to_pos = function
  |`Pair(`Int(a), `Int(b)) -> Position.create (a,b)
  | _ -> assert false

let main_script (env, ep) = 
  match eval_seq env ep#main with
  |`Soldier(u) -> u
  | _ -> assert false

let move_script (env, ep) u = 
  match eval_seq env (ep#move u#name) with
  |`List(l) -> List.map pair_to_pos l
  | _ -> assert false

let attack_script (env, ep) u = 
  match eval_seq env (ep#attack u#name) with
  |`Soldier(u) -> u
  | _ -> assert false
