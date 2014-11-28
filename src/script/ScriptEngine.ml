type value_type = [
  `Int_t    |
  `Unit_t   |
  `String_t |
  `Bool_t   |
  `Alpha_t of int |
  `List_t  of value_type |
  `Array_t of value_type |
  `Fun_t   of value_type * value_type |
  `Pair_t  of value_type * value_type
]

type value = [
  `Int    of int              |
  `Unit                       |
  `String of string           |
  `Bool   of bool             |
  `List   of value list       |
  `Array  of value array      |
  `Fun    of (value -> value) |
  `Pair   of value * value
]

exception Script_value_not_found

let value_table = Hashtbl.create 13

let expose f t s = 
  print_endline ("  [\027[33mexposed\027[0m] " ^ s);  
  Hashtbl.add value_table s (f,t)

let type_of s = 
  try 
    snd (Hashtbl.find value_table s)
  with
    |Not_found -> raise Script_value_not_found

let value_of s = 
  try
    fst (Hashtbl.find value_table s)
  with
    |Not_found -> raise Script_value_not_found

