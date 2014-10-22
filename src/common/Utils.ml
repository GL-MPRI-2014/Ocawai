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
