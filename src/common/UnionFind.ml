
type 'a t = Node of 'a node | Leaf of 'a list
and 'a node  = {mutable parent : 'a t}

let make_sing data = {parent = Leaf [data]}

let union x y =
  if x != y then
  (
    let data_x, data_y = match x.parent, y.parent with
      | Leaf dx, Leaf dy -> dx, dy
      | _ -> assert(false) in
    x.parent <- Leaf (data_x @ data_y);
    y.parent <- Node(x)
  )

let rec find x = match x.parent with
  | Node p -> find p
  | Leaf _ -> x

let union_gen x y = union (find x) (find y)

let get_data x = match x.parent with
  | Node _ -> assert(false)
  | Leaf d -> d

let get_data_gen x = get_data (find x)

(*
let test () =
  let a = make_sing 0 in
  let b = make_sing 1 in
  let c = make_sing 2 in
  assert(find a = a);
  assert(find a <> find b);
  assert(get_data a = [0]);
  union a b;
  assert(find a = find b);
  assert(get_data_gen a = [0; 1] || get_data_gen a = [1; 0]);
  union_gen a b;
  assert(find a = find b);
  union_gen b c;
  assert(find a = find c);
  union_gen c a;
  assert(find b = find c);
  assert(find a = find c)

let () = test ()
*)

