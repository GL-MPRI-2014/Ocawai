type t = int * int

let create p = p

let topair p = p

let clamp (a,b) (mina,minb) (maxa, maxb) = 
  let clamp_aux x minx maxx = max (min x maxx) minx in
  (clamp_aux a mina maxa, clamp_aux b minb maxb)

let left (a,b) = (a-1,b)

let right (a,b) = (a+1,b)

let down (a,b) = (a,b+1)

let up (a,b) = (a,b-1)

let add (a,b) (a',b') = (a + a', b + b')

let diff (a,b) (a',b') = (a - a', b - b')

let square (a,b) (a',b') = 
  let rec sq_aux (x,y) = 
    if x > a' then sq_aux (a,y+1)
    else if y > b' then []
    else (x,y)::(sq_aux (x+1,y))
  in sq_aux (a,b)

let circle c r = 
  if r = 0 then [c] 
  else begin
    let l = ref [] in 
    for x = -r + 1 to r - 1 do
      l := (x, r - (abs x)) :: (x, (abs x) - r) :: !l
    done;
    l := (-r, 0) :: (r, 0) :: !l;
    List.map (fun p -> add p c) !l
  end

let rec filled_circle c = function
  | 0 -> [c]  
  | r -> (circle c r) @ (filled_circle c (r-1))

let neighbours l = 
  (* add an element to a list without duplication *)
  let rec add_elt elt = function
    |[] -> [elt]
    |t::q when t = elt -> t::q 
    |t::q when t > elt -> elt::t::q
    |t::q -> t::(add_elt elt q)
  in 
  (* check if an element is in a list *)
  let rec is_in elt = function
    |[] -> false
    |t::q  -> t = elt || is_in elt q
  in
  let rec neigh_aux = function
    |[] -> []
    |t::q -> 
      neigh_aux q 
      |> add_elt (up t) 
      |> add_elt (right t)
      |> add_elt (down t)
      |> add_elt (left t)
  in 
  List.filter (fun e -> not (is_in e l)) (neigh_aux l)
      
let project p c i = 
  let (a,b) = diff p c in
  (a * i, b * i)
