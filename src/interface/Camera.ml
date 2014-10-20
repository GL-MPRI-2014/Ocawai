let foi2D (a,b) = (float_of_int a, float_of_int b)
let iof2D (a,b) = (int_of_float a, int_of_float b)
let clamp2D (a,b) (mina, minb) (maxa, maxb) = 
  (min (max a mina) maxa, min (max b minb) maxb)

class camera ~tile_size ~w ~h ~maxpos = object(self)

  val mutable center = foi2D
    (40 * tile_size + tile_size / 2, 40 * tile_size + tile_size / 2)

  method private max_coordinates = 
    let (maxa, maxb) = Position.topair maxpos in 
    ((maxa + 1) * tile_size - 1, (maxb + 1) * tile_size - 1)

  method cursor = 
    let (cx, cy) = iof2D center in
    Position.create 
    (cx / tile_size, cy / tile_size)

  method project p =
    let (cx,cy) = iof2D center in 
    let (x,y) = Position.topair p in 
    let (dx,dy) = (x * tile_size - cx, y * tile_size - cy) in
    (dx + (w + tile_size)/2, dy + (h + tile_size)/2)

  method top_left =
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp
      (Position.diff self#cursor p)
      (Position.create (0,0))
      maxpos

  method bottom_right =
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp
      (Position.add self#cursor p)
      (Position.create (0,0))
      maxpos

  method tile_size = tile_size

  method move (vx, vy) = 
    let (a,b) = center in 
    center <- clamp2D
      (a +. vx, b +. vy)
      (0., 0.)
      (foi2D self#max_coordinates)
end
