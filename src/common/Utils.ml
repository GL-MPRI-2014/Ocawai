let iof2D (a,b) = (int_of_float a, int_of_float b)

let foi2D (a,b) = (float_of_int a, float_of_int b)

let clamp2D (a,b) (mina,minb) (maxa, maxb) = 
  (min (max a mina) maxa, min (max b minb) maxb)

let addf2D (a,b) (c,d) = (a +. c, b +. d)

let subf2D (a,b) (c,d) = (a -. c, b -. d)

let add2D (a,b) (c,d) = (a + c, b + d)

let sub2D (a,b) (c,d) = (a - c, b - d)

let (>?) opt f = match opt with
  |None -> ()
  |Some(s) -> f s

let shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort (fun (x,_) (y,_) -> compare x y) nd in
  List.map snd sond

let init_string n f =
  if n < 0 || n > Sys.max_string_length then 
    raise (Invalid_argument "init_string");
  let s = String.make n ' ' in
  for i = 0 to n - 1 do
    s.[i] <- f i
  done; s

module type PrioQueue =
    sig
      type priority = int
      type 'a queue
      val empty : 'a queue
      val insert : 'a queue -> int -> 'a -> 'a queue
      val extract : 'a queue -> int * 'a * 'a queue
      exception Queue_is_empty
    end
    
module PriorityQueue : PrioQueue =
   struct
      type priority = int
      type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
      let empty = Empty
      let rec insert queue prio elt =
        match queue with
        | Empty -> Node(prio, elt, Empty, Empty)
        | Node(p, e, left, right) ->
            if prio <= p
            then Node(prio, elt, insert right p e, left)
            else Node(p, e, insert right prio elt, left)
      exception Queue_is_empty
      let rec remove_top = function
        | Empty -> raise Queue_is_empty
        | Node(prio, elt, left, Empty) -> left
        | Node(prio, elt, Empty, right) -> right
        | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                          (Node(rprio, relt, _, _) as right)) ->
            if lprio <= rprio
            then Node(lprio, lelt, remove_top left, right)
            else Node(rprio, relt, left, remove_top right)
      let extract = function
          Empty -> raise Queue_is_empty
        | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
    end
