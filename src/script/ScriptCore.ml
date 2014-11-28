(** Exposes some functions to the script engine *)

let scr_or = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Bool(a), `Bool(b)) -> `Bool(a || b)
      | _ -> assert false
    )
  )

let scr_and = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Bool(a), `Bool(b)) -> `Bool(a && b)
      | _ -> assert false
    )
  )

let scr_not = 
  `Fun(fun a -> 
    match a with
    |`Bool(a) -> `Bool(not a)
    | _ -> assert false
  )

let scr_gt = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Bool(a > b)
      | _ -> assert false
    )
  )

let scr_lt = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Bool(a < b)
      | _ -> assert false
    )
  )

let scr_eq = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Bool(a = b)
      | _ -> assert false
    )
  )

let scr_le = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Bool(a <= b)
      | _ -> assert false
    )
  )

let scr_ge = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Bool(a >= b)
      | _ -> assert false
    )
  )

let scr_mul = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Int(a * b)
      | _ -> assert false
    )
  )

let scr_add = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Int(a + b)
      | _ -> assert false
    )
  )

let scr_sub = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Int(a - b)
      | _ -> assert false
    )
  )

let scr_div = 
  `Fun(fun a -> 
    `Fun (fun b -> 
      match (a,b) with
      |(`Int(a), `Int(b)) -> `Int(a / b)
      | _ -> assert false
    )
  )

let scr_printf = 
  `Fun(function
    |`String(s) -> Printf.printf "%s" s; `Unit
    | _ -> assert false
  )

let scr_printi = 
  `Fun(function
    |`Int(i) -> Printf.printf "%i" i; `Unit
    | _ -> assert false
  )

let scr_listhd = 
  `Fun(function
    |`List(t::q) -> t
    | _ -> assert false
  )

let scr_listtl = 
  `Fun(function
    |`List(t::q) -> `List q
    | _ -> assert false
  )

let () = 
  ScriptEngine.expose scr_or  (`Fun_t(`Bool_t, `Fun_t(`Bool_t, `Bool_t))) "_or" ;
  ScriptEngine.expose scr_and (`Fun_t(`Bool_t, `Fun_t(`Bool_t, `Bool_t))) "_and";
  ScriptEngine.expose scr_gt  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_gt" ;
  ScriptEngine.expose scr_lt  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_lt" ;
  ScriptEngine.expose scr_eq  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_eq" ;
  ScriptEngine.expose scr_ge  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_ge" ;
  ScriptEngine.expose scr_le  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_le" ;
  ScriptEngine.expose scr_mul (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_mul";
  ScriptEngine.expose scr_add (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_add";
  ScriptEngine.expose scr_sub (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_sub";
  ScriptEngine.expose scr_div (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_div";
  ScriptEngine.expose scr_not (`Fun_t(`Bool_t, `Bool_t)) "_not";
  ScriptEngine.expose scr_printf (`Fun_t(`String_t, `Unit_t)) "print_string";
  ScriptEngine.expose scr_printi (`Fun_t(`Int_t   , `Unit_t)) "print_int";
  ScriptEngine.expose scr_listhd (`Fun_t(`List_t (`Alpha_t(0)), `Alpha_t(0))) "list_hd";
  ScriptEngine.expose scr_listtl (`Fun_t(`List_t (`Alpha_t(0)), `List_t(`Alpha_t(0)))) "list_tl";



