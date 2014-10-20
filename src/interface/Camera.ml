let foi2D (a,b) = (float_of_int a, float_of_int b)
let iof2D (a,b) = (int_of_float a, int_of_float b)
let clamp2D (a,b) (mina, minb) (maxa, maxb) =
  (min (max a mina) maxa, min (max b minb) maxb)

class camera ~tile_size ~w ~h ~maxpos = object(self)


  (* Double hardcoding... *)
  val mutable center = foi2D
    (40 * tile_size + tile_size / 2, 40 * tile_size + tile_size / 2)

  val cursor = new Cursor.cursor ~position:(Position.create (40,40))

  method private max_coordinates =
    let (maxa, maxb) = Position.topair maxpos in
    ((maxa + 1) * tile_size - 1, (maxb + 1) * tile_size - 1)

  method cursor = cursor

  method project p =
    let (cx,cy) = iof2D center in
    let (x,y) = Position.topair p in
    let (dx,dy) = (x * tile_size - cx, y * tile_size - cy) in
    (dx + (w + tile_size)/2, dy + (h + tile_size)/2)

  method top_left =
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp
      (Position.diff cursor#position p)
      (Position.create (0,0))
      maxpos

  method bottom_right =
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp
      (Position.add cursor#position p)
      (Position.create (0,0))
      maxpos

  method tile_size = tile_size

  method move (vx, vy) =
    let (a,b) = center in
    center <- clamp2D
      (a +. vx, b +. vy)
      (0., 0.)
      (foi2D self#max_coordinates);
    let (cx, cy) = iof2D center in
    cursor#set_position
      (Position.create (cx / tile_size, cy / tile_size))
end
