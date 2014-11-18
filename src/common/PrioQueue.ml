type priority = int

type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue * bool
exception Queue_is_empty

let empty = Empty

let rec push queue prio elt =
  match queue with
  | Empty -> Node(prio, elt, Empty, Empty, true)
  | Node(p, e, Empty, Empty, true) ->
      let (p1,e1,p2,e2) = if prio <= p then (prio,elt,p,e) else (p,e,prio,elt) in
      Node(p1, e1, push Empty p2 e2, Empty, false)
  | Node(p, e, (Node(_,_,_,_,cl) as left), right, complete) ->
      let (p1,e1,p2,e2) = if prio <= p then (prio,elt,p,e) else (p,e,prio,elt) in
      if complete then
        Node(p1, e1, push left p2 e2, right, false)
      else
        if not cl then
          Node(p1, e1, push left p2 e2, right, false)
        else
          let r = push right p2 e2 in
          Node(p1, e1, left, r, match r with | Empty -> assert false | Node(_,_,_,_,b) -> b )
  | _ -> assert false

let pop queue = 
  let rec remove_last = function 
  | Empty -> raise Queue_is_empty
  | Node(p, e, Empty, Empty, true) as n -> p,e,Empty,true
  | Node(p, e, (Node(_,_,_,_,cl) as left), right, complete) ->
      if complete then
        let (np,ne,r,b) = remove_last right in
        (np, ne, Node(p, e, left, r, false),false)
      else
        if not cl then
          let (np,ne,r,b) = remove_last left in
          (np, ne, Node(p, e, r, right, b),b)
        else
          let (np,ne,r,b) = remove_last right in
          (np, ne, Node(p, e, left, r, false),false)
  | _ -> assert false
  in
  let rec repare = function
  | Empty -> assert false
  | Node(prio, elt, Empty, Empty, c) as n -> n
  | Node(prio, elt, (Node(lprio, lelt, ll, lr, lc)), Empty, c) as n ->
      if prio > lprio then
        Node(lprio, lelt, repare (Node(prio,elt,ll,lr,lc)), Empty, c)
      else n
  | Node(prio, elt, Empty, (Node(rprio, relt, rl, rr, rc)), c) as n ->
      if prio > rprio then
        Node(rprio, relt, Empty, repare (Node(prio,elt,rl,rr,rc)), c)
      else n
  | Node(prio, elt, (Node(lprio, lelt, ll, lr, lc) as left),
                    (Node(rprio, relt, rl, rr, rc) as right), c) as n ->
      if prio > min lprio rprio then
        if lprio <= rprio then
          Node(lprio, lelt, repare (Node(prio,elt,ll,lr,lc)), right, c)
        else
          Node(rprio, relt, left, repare (Node(prio,elt,rl,rr,rc)), c)
      else n
  in
  let (p,e,q,_) = remove_last queue in
  match q with
  | Empty -> (p,e,Empty)
  | Node(p0,e0,left,right,c) -> 
  (p0, e0, repare (Node(p,e,left,right,c)) )

let top = function
| Empty -> raise Queue_is_empty
| Node(p,e,_,_,_) -> (p,e)
      
