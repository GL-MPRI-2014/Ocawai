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

let () =
  Log.set_log_level Log.DEBUG;
  Log.set_output stdout;
  Log.color_on()

let check_validity f path x =
  if f x then None
  else Some (Ag_util.Validation.error path)
