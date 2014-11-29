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

let scr_listempty = 
  `Fun(function
    |`List([]) -> `Bool(true)
    |`List(_ ) -> `Bool(false)
    | _ -> assert false
  )

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
    |`Soldier(u) ->
      let self = 
        ScriptValues.value_of "self"
        |> get_logic_player
      in
      let plist = 
        match ScriptValues.value_of "players" with
        |`List(l) -> List.map get_logic_player l
        | _       -> assert false
      in
      let map = 
        match ScriptValues.value_of "map" with
        |`Map(m) -> m
        | _      -> assert false
      in
      fst (Logics.accessible_positions u self plist map)
      |> List.map position_to_pair
      |> fun l -> `List l
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

let intpair = `Pair_t (`Int_t, `Int_t)

let init () = 
  (* Functions on base types *)
  ScriptValues.expose scr_or  (`Fun_t(`Bool_t, `Fun_t(`Bool_t, `Bool_t))) "_or" ;
  ScriptValues.expose scr_and (`Fun_t(`Bool_t, `Fun_t(`Bool_t, `Bool_t))) "_and";
  ScriptValues.expose scr_gt  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_gt" ;
  ScriptValues.expose scr_lt  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_lt" ;
  ScriptValues.expose scr_eq  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_eq" ;
  ScriptValues.expose scr_ge  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_ge" ;
  ScriptValues.expose scr_le  (`Fun_t(`Int_t , `Fun_t(`Int_t , `Bool_t))) "_le" ;
  ScriptValues.expose scr_mul (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_mul";
  ScriptValues.expose scr_add (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_add";
  ScriptValues.expose scr_sub (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_sub";
  ScriptValues.expose scr_div (`Fun_t(`Int_t , `Fun_t(`Int_t , `Int_t ))) "_div";
  ScriptValues.expose scr_not (`Fun_t(`Bool_t, `Bool_t)) "_not";
  ScriptValues.expose scr_printf (`Fun_t(`String_t, `Unit_t)) "print_string";
  ScriptValues.expose scr_printi (`Fun_t(`Int_t   , `Unit_t)) "print_int";
  ScriptValues.expose scr_listhd (`Fun_t(`List_t (`Alpha_t(0)), `Alpha_t(0))) "list_hd";
  ScriptValues.expose scr_listtl (`Fun_t(`List_t (`Alpha_t(0)), `List_t(`Alpha_t(0)))) "list_tl";
  ScriptValues.expose scr_fst    (`Fun_t(`Pair_t (`Alpha_t(0), `Alpha_t(1)), `Alpha_t(0))) "fst";
  ScriptValues.expose scr_snd    (`Fun_t(`Pair_t (`Alpha_t(0), `Alpha_t(1)), `Alpha_t(1))) "snd";
  ScriptValues.expose scr_add2   (`Fun_t(intpair, `Fun_t(intpair, intpair))) "add2D";
  ScriptValues.expose scr_sub2   (`Fun_t(intpair, `Fun_t(intpair, intpair))) "sub2D";
  ScriptValues.expose scr_prop2  (`Fun_t(`Int_t, `Fun_t(intpair, intpair))) "prop2D";
  ScriptValues.expose scr_listempty (`Fun_t(`List_t (`Alpha_t(0)), `Bool_t)) "list_empty";
  (* Functions on units/map *)
  ScriptValues.expose scr_hasplayed (`Fun_t(`Soldier_t, `Bool_t)) "unit_has_played";
  ScriptValues.expose scr_unitpos (`Fun_t(`Soldier_t, intpair)) "unit_position";
  ScriptValues.expose scr_accessibles (`Fun_t(`Soldier_t, `List_t(intpair))) "accessible_positions";
  ScriptValues.expose scr_armyof (`Fun_t(`Player_t, `List_t(`Soldier_t))) "army_of"



