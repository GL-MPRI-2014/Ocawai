open Types
open ScriptEngine

type var_environment = (string * value ref) list


exception Unbound_variable of string

exception Unbound_function of string


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

end


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
    try ScriptEngine.value_of s 
    with |ScriptEngine.Script_value_not_found -> 
      raise (Unbound_variable s)
  end

let rec eval_proc entries = function
  |Move (p,_,_) -> entries#add_move p
  |Attack (p,_,_) -> entries#add_attack p
  |Main(p,_,_) -> entries#add_main p
  |Init(p,_,_) -> entries#add_init p

and create_lambda env args seq = 
  match args with
  |[] -> eval_seq env seq 
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
      new_value s (create_lambda env args seq) env

and eval_seq env = function
  |Seq((decl, seq),_,_) -> 
      let env' = eval_decl env decl in 
      eval_seq env' seq
  |Return(v, _, _) -> 
      eval_value env v

and eval_prog env entries = function
  |Globseq ((decl, prog), _, _) -> 
      let env' = eval_decl env decl in
      eval_prog env' entries prog
  |Procseq ((proc, prog), _, _) -> 
      eval_proc entries proc;
      eval_prog env entries prog
  |Empty -> env


let interprete prog = eval_prog [] (new entrypoints) prog
