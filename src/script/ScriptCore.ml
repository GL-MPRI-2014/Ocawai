(** Exposes some functions to the script engine *)

exception Do_nothing
exception End_turn

let expose f t s =
  ScriptValues.expose f t s ;
  Checker.expose t s

(** Various functions *)
let scr_rand = 
  `Fun(function
    |`Int(n) -> `Int(Random.int n)
    | _ -> assert false
  )

(** Boolean functions *)
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


(** Arithmetics functions *)
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


(** Printing functions *)
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


(** List funtions *)
let scr_listhd =
  `Fun(function
    |`List(t::q) -> t
    |`List([]) -> raise (Invalid_argument "Script : List.hd")
    | _ -> assert false
  )

let scr_listtl =
  `Fun(function
    |`List(t::q) -> `List q
    |`List([]) -> raise (Invalid_argument "Script : List.tl")
    | _ -> assert false
  )

let scr_listempty =
  `Fun(function
    |`List([]) -> `Bool(true)
    |`List(_::_) -> `Bool(false)
    | _ -> assert false
  )

let scr_listmem =
  `Fun(fun l ->
    `Fun(fun m ->
      match l with
      |`List(l) -> `Bool(List.mem m l)
      | _ -> assert false
    )
  )

let scr_listnth = 
  `Fun(fun l ->
    `Fun(fun n ->
      match (l,n) with
      |`List(l), `Int(n) -> List.nth l n
      | _ -> assert false
    )
  )

let scr_listlength = 
  `Fun(function
    |`List(l) -> `Int(List.length l)
    | _ -> assert false
  )


(** Pair functions *)
let scr_fst =
  `Fun(function
    |`Pair(a,b) -> a
    | _ -> assert false
  )

let scr_snd =
  `Fun(function
    |`Pair(a,b) -> b
    | _ -> assert false
  )

let scr_add2 =
  `Fun(fun a ->
    `Fun(fun b ->
      match (a,b) with
      |(`Pair(`Int(i), `Int(j)), `Pair(`Int(i'), `Int(j'))) ->
          `Pair(`Int(i+i'), `Int(j+j'))
      | _ -> assert false
    )
  )

let scr_sub2 =
  `Fun(fun a ->
    `Fun(fun b ->
      match (a,b) with
      |(`Pair(`Int(i), `Int(j)), `Pair(`Int(i'), `Int(j'))) ->
          `Pair(`Int(i-i'), `Int(j-j'))
      | _ -> assert false
    )
  )

let scr_prop2 =
  `Fun(fun a ->
    `Fun(fun b ->
      match (a,b) with
      |(`Int(k), `Pair(`Int(i), `Int(j))) ->
          `Pair(`Int(i*k), `Int(j*k))
      | _ -> assert false
    )
  )


(** Gameplay functions *)
let scr_hasplayed =
  `Fun(function
    |`Soldier(u) -> `Bool u#has_played
    | _ -> assert false
  )

let scr_unitpos =
  `Fun(function
    |`Soldier(u) ->
      let (a,b) = Position.topair u#position in
      `Pair(`Int a, `Int b)
    | _ -> assert false
  )

let get_logic_player = function
  |`Player(p) -> p
  | _ -> assert false

let position_to_pair p =
  let (a,b) = Position.topair p in
  `Pair(`Int a, `Int b)

let scr_accessibles =
  `Fun(function
    |`Soldier(u) -> `Fun(function
      |`Player(s) -> `Fun(function
        |`List(l) -> `Fun(function
          |`Map(m) -> 
            fst (Logics.available_positions u s (List.map get_logic_player l) m)
            |> List.map position_to_pair
            |> fun l -> `List l
          | _ -> assert false
        )
        | _ -> assert false
      )
      | _ -> assert false
    )
    | _ -> assert false
  )

let scr_armyof =
  `Fun(function
    |`Player(p) ->
        p#get_army
        |> List.map (fun u -> `Soldier u)
        |> fun l -> `List l
    | _ -> assert false
  )

let scr_dijkstra =
  `Fun(function
    |`Soldier(u) -> `Fun(function
      |`Map(m) -> `Fun(function
        |`Pair(`Int(a), `Int(b)) ->
            Path.dijkstra m u#position u#movement_type (Position.create (a,b))
            |> (function
                |Some(i,p) -> Path.get_move p
                | _ -> assert false)
            |> List.map position_to_pair
            |> fun l -> `List l
        | _ -> assert false
        )
      | _ -> assert false
      )
    | _ -> assert false
    )

let scr_donothing = 
  `Fun(function
    |`Unit -> raise Do_nothing
    | _ -> assert false
  )

let scr_endturn = 
  `Fun(function
    |`Unit -> raise End_turn
    | _ -> assert false
  )

let intpair = `Pair_t (`Int_t, `Int_t)

let init () =
  (* Functions on base types *)
  expose scr_or  (`Fun_t(`Bool_t, `Fun_t(`Bool_t, `Bool_t))) "_or" ;
  expose scr_and (`Fun_t(`Bool_t, `Fun_t(`Bool_t, `Bool_t))) "_and";
  expose scr_gt  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_gt" ;
  expose scr_lt  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_lt" ;
  expose scr_eq  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_eq" ;
  expose scr_ge  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_ge" ;
  expose scr_le  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_le" ;
  expose scr_mul (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_mul";
  expose scr_add (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_add";
  expose scr_sub (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_sub";
  expose scr_div (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_div";
  expose scr_not (`Fun_t(`Bool_t, `Bool_t)) "_not";
  expose scr_printf (`Fun_t(`String_t, `Unit_t)) "print_string";
  expose scr_printi (`Fun_t(`Int_t   , `Unit_t)) "print_int";
  expose scr_listhd (`Fun_t(`List_t (`Alpha_t(0)), `Alpha_t(0))) "list_hd";
  expose scr_listtl (`Fun_t(`List_t (`Alpha_t(0)), `List_t(`Alpha_t(0)))) "list_tl";
  expose scr_listmem(`Fun_t(`List_t (`Alpha_t(0)), `Fun_t(`Alpha_t(0), `Bool_t))) "list_mem";
  expose scr_listnth(`Fun_t(`List_t (`Alpha_t(0)), `Fun_t(`Int_t, `Alpha_t(0)))) "list_nth";
  expose scr_fst    (`Fun_t(`Pair_t (`Alpha_t(0), `Alpha_t(1)), `Alpha_t(0))) "fst";
  expose scr_snd    (`Fun_t(`Pair_t (`Alpha_t(0), `Alpha_t(1)), `Alpha_t(1))) "snd";
  expose scr_add2   (`Fun_t(intpair, `Fun_t(intpair, intpair))) "add2D";
  expose scr_sub2   (`Fun_t(intpair, `Fun_t(intpair, intpair))) "sub2D";
  expose scr_prop2  (`Fun_t(`Int_t, `Fun_t(intpair, intpair))) "prop2D";
  expose scr_listempty (`Fun_t(`List_t (`Alpha_t(0)), `Bool_t)) "list_empty";
  expose scr_listlength(`Fun_t(`List_t (`Alpha_t(0)), `Int_t)) "list_length";
  (* Functions on units/map *)
  expose scr_hasplayed (`Fun_t(`Soldier_t, `Bool_t)) "unit_has_played";
  expose scr_unitpos (`Fun_t(`Soldier_t, intpair)) "unit_position";
  expose scr_accessibles (`Fun_t(`Soldier_t, `Fun_t(`Player_t,
    `Fun_t(`List_t(`Player_t), `Fun_t(`Map_t, `List_t(intpair)))))) "accessible_positions";
  expose scr_armyof (`Fun_t(`Player_t, `List_t(`Soldier_t))) "army_of";
  expose scr_dijkstra (`Fun_t(`Soldier_t, `Fun_t(`Map_t, `Fun_t(intpair,`List_t(intpair))))) "dijkstra_to";
  expose scr_rand (`Fun_t(`Int_t, `Int_t)) "rand";
  expose scr_endturn (`Fun_t(`Unit_t, `Alpha_t(0))) "end_turn";
  expose scr_donothing (`Fun_t(`Unit_t, `Alpha_t(0))) "do_nothing";
