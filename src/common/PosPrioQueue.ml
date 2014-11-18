type priority = int
type index = ((bool list) option) array array
type t_tree = Empty | Node of priority * Position.t * t_tree * t_tree * bool
type t = (t_tree ref) * index
exception Queue_is_empty

let empty w h = (ref Empty,Array.make_matrix w h None)

let is_empty (r,_) = !r = Empty

let set_empty (r,_) = r := Empty

let p_none = -1

(* Manipulate an array indexed by positions *)
let get a p = let (a1,a2) = Position.topair p in a.(a1).(a2)
let set a p v = let (a1,a2) = Position.topair p in a.(a1).(a2) <- v
let set_root a p = set a p (Some [])
let add a p b = set a p (Some (b::(match get a p with | None -> assert false | Some l -> l)))
let rem a p = set a p None
let behead a p = set a p (match get a p with | None | Some [] -> assert false | Some(p::q) -> Some q)
let path a p = List.rev (match get a p with | None -> assert false | Some l -> l)
let compare_si p1 p2 = match (p1,p2) with | a,b when a = b && b = p_none -> false | _,b when b = p_none -> true | a,_ when a = p_none -> false | _ -> p1 < p2
let compare_i p1 p2 = match (p1,p2) with | a,b when a = b && b = p_none -> true | _,b when b = p_none -> true | a,_ when a = p_none -> false | _ -> p1 <= p2
let compare_ss p1 p2 = match (p1,p2) with | a,b when a = b && b = p_none -> false | _,b when b = p_none -> false | a,_ when a = p_none -> true | _ -> p1 > p2
let compare_s p1 p2 = match (p1,p2) with | a,b when a = b && b = p_none -> true | _,b when b = p_none -> false | a,_ when a = p_none -> true | _ -> p1 >= p2
let min_p p1 p2 = match (p1,p2) with | a,b when a = b && b = p_none -> (-1) | _,b when b = p_none -> p1 | a,_ when a = p_none -> p2 | _ -> min p1 p2
let matrix_foreach f =
  Array.iteri (fun x -> Array.iteri (f x))

let print_queue queue =
  let tab n = for i = 0 to n do print_string " "; done in
  let rec aux n = function
  | Empty -> tab n;print_string "e"
  | Node(p,e,l,r,c) -> let (a,b) = Position.topair e in
      tab n;print_endline ("{"^(string_of_int p)^" :("^(string_of_int a)^","^(string_of_int b)^")");
      tab n;print_endline "left";aux (n+1) l;print_newline();tab n;print_endline "right";aux (n+1) r;print_newline();tab n;print_string (if c then "c 1}" else "c 0}")
  in
  aux 0 queue

let print (refqueue,ind) =
  print_endline "tree :";
  print_queue !refqueue;
  print_endline "\npaths :";
  matrix_foreach (fun x y l -> match l with | None -> () | Some ll -> print_int x;print_string " , ";print_int y;print_string " : [";List.iter (fun b -> print_string (if b then "1 " else "0 ");) (List.rev ll);print_endline "]" ) ind

let push (refqueue,ind) prio elt =
  let queue = !refqueue in
  let rec push_aux queue prio elt =
    match queue with
    | Empty -> Node(prio, elt, Empty, Empty, true)
    | Node(p, e, Empty, Empty, true) ->
        let (p1,e1,p2,e2) = if compare_i prio p then (prio,elt,p,e) else (p,e,prio,elt) in
        add ind e2 false;
        Node(p1, e1, push_aux Empty p2 e2, Empty, false)
    | Node(p, e, (Node(_,_,_,_,cl) as left), right, complete) ->
        let (p1,e1,p2,e2) = if compare_i prio p then (prio,elt,p,e) else (p,e,prio,elt) in
        if complete then
        begin
          add ind e2 false;
          Node(p1, e1, push_aux left p2 e2, right, false)
        end
        else
          if not cl then
          begin
            add ind e2 false;
            Node(p1, e1, push_aux left p2 e2, right, false)
          end
          else
          begin
            add ind e2 true;
            let r = push_aux right p2 e2 in
            Node(p1, e1, left, r, match r with | Empty -> assert false | Node(_,_,_,_,b) -> b )
          end
    | _ -> assert false
  in set_root ind elt; refqueue := push_aux queue prio elt

let pop (refqueue,ind) = 
  let queue = !refqueue in
  let rec remove_last queue = match queue with 
  | Empty -> raise Queue_is_empty
  | Node(p, e, Empty, Empty, true) -> p,e,Empty,true
  | Node(p, e, left, Empty, complete) ->
      let (np,ne,l,b) = remove_last left in
      (np, ne, Node(p, e, l, Empty, b),b)
  | Node(p, e, (Node(_,_,_,_,lc) as left), (Node(_,_,_,_,rc) as right), complete) ->
      if complete || (lc && (not rc))then
        let (np,ne,r,b) = remove_last right in
        (np, ne, Node(p, e, left, r, false),false)
      else 
        let (np,ne,l,b) = remove_last left in
        (np, ne, Node(p, e, l, right, b),b)
  | _ -> assert false
  in
  let rec repare = function
  | Empty -> assert false
  | Node(prio, elt, Empty, Empty, c) as n -> n
  | Node(prio, elt, (Node(lprio, lelt, ll, lr, lc)), Empty, c) as n ->
      if compare_ss prio lprio then
      begin
        behead ind lelt;
        add ind elt false;
        Node(lprio, lelt, repare (Node(prio,elt,ll,lr,lc)), Empty, c)
      end
      else n
  | Node(prio, elt, Empty, (Node(rprio, relt, rl, rr, rc)), c) as n ->
      if compare_ss prio rprio then
      begin
        behead ind relt;
        add ind elt true;
        Node(rprio, relt, Empty, repare (Node(prio,elt,rl,rr,rc)), c)
      end
      else n
  | Node(prio, elt, (Node(lprio, lelt, ll, lr, lc) as left),
                    (Node(rprio, relt, rl, rr, rc) as right), c) as n ->
      if compare_ss prio (min_p rprio lprio) then
      begin
        if compare_i lprio rprio then
        begin
          behead ind lelt;
          add ind elt false;
          Node(lprio, lelt, repare (Node(prio,elt,ll,lr,lc)), right, c)
        end
        else
        begin
          behead ind relt;
          add ind elt true;
          Node(rprio, relt, left, repare (Node(prio,elt,rl,rr,rc)), c)
        end
      end
      else n
  in
  let (p,e,q,_) = remove_last queue in
  set_root ind e;
  match q with
  | Empty -> refqueue := Empty;(p,e)
  | Node(p0,e0,left,right,c) -> 
      rem ind e0; refqueue := repare (Node(p,e,left,right,c)); (p0,e0)

let top (refqueue,_) = match !refqueue with
| Empty -> raise Queue_is_empty
| Node(p,e,_,_,_) -> (p,e)

let decrease_priority (refqueue,ind) prio elt =
  let queue = !refqueue in
  let rec aux = function
  | [] , Node(p,elt,left,right,c) -> Node(prio,elt,left,right,c)
  | false::q, Node(p,e,left,right,c) ->
      begin
        match aux (q,left) with
        | Empty -> assert false
        | Node(lp,le,ll,lr,lc) as n ->
            if compare_si lp p then
            begin
              behead ind le;
              add ind e false;
              Node(lp,le,Node(p,e,ll,lr,lc),right,c)
            end
            else
              Node(p,e,n,right,c)
      end
  | true::q, Node(p,e,left,right,c) ->
      begin
        match aux (q,right) with
        | Empty -> assert false
        | Node(rp,re,rl,rr,rc) as n->
            if compare_si rp p then
            begin
              behead ind re;
              add ind e true;
              Node(rp,re,left,Node(p,e,rl,rr,rc),c)
            end
            else
              Node(p,e,left,n,c)
      end
  | _ -> assert false
  in refqueue := aux(path ind elt,queue)

