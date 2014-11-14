type t = int * int

let compare (x0,y0) (x1,y1) =
  match Pervasives.compare x0 x1 with
  | 0 -> Pervasives.compare y0 y1
  | c -> c

let create p = p

let topair p = p

let clamp (a,b) (mina,minb) (maxa, maxb) = 
  let clamp_aux x minx maxx = max (min x maxx) minx in
  (clamp_aux a mina maxa, clamp_aux b minb maxb)

let out_of_bounds p minp maxp = 
  clamp p minp maxp <> p

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

let range center minr maxr = 
  let sq = filled_circle center maxr in 
  List.filter (fun p -> 
    let (dx, dy) = diff p center in
    let dist = abs dx + abs dy in
    dist >= minr) sq
      
let project p c i = 
  let (a,b) = diff p c in
  (a * i, b * i)

let dist (x1,y1) (x2,y2) =
  (abs (x2-x1)) + (abs (y2-y1)) 

