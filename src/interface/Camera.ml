let foi2D (a,b) = (float_of_int a, float_of_int b)
let iof2D (a,b) = (int_of_float a, int_of_float b)
let clamp2D (a,b) (mina, minb) (maxa, maxb) =
  (min (max a mina) maxa, min (max b minb) maxb)

class camera ~tile_size ~w ~h ~maxpos = object(self)

  val cursor = new Cursor.cursor ~position:(Position.create (40,40))

  val mutable offset = (0, 0)

  method cursor = cursor

  method project p =
    let (x,y) = Position.topair (Position.diff p cursor#position) in
    let (dx,dy) = (x * tile_size + (fst offset), 
                   y * tile_size + (snd offset)) in
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

  method move depl =
    cursor#set_position
      (Position.clamp 
        (Position.add (Position.create depl) cursor#position)
        (Position.create (0,0))
        maxpos)
       
  method set_position pos = 
    cursor#set_position 
      (Position.clamp pos (Position.create (0,0)) maxpos)
end
