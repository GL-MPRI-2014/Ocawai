(* TODO : add map size to camera *)

class camera ~tile_size ~w ~h = object(self)

  val mutable cursor = Position.create (4,4)

  method set_cursor p = cursor <- p

  method cursor = cursor

  (* For now, view is centered on the cursor, independantly of its position *)
  method project p = 
    let (x,y) = Position.project p cursor tile_size in
    (x + (w - tile_size)/2, y + (h - tile_size)/2)

  (* Use this one if you want a view centered on the cursor, except if it
   * is too close from top or left border, so that we never see the 
   * "exterior" of the map. *)
  
  (*method project p = 
    let (offx,offy) = Position.project self#top_left cursor tile_size in
    let (x,y) = Position.project p self#top_left tile_size in 
    (x + offx, y + offy)*)

  method top_left = 
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    Position.clamp 
      (Position.diff cursor p)
      (Position.create (0,0))
      (* Here we should clamp to map size ... *)
      cursor

  method bottom_right = 
    let p = Position.create (w/(2*tile_size) + 1, h/(2*tile_size) + 1) in
    (* Here too *)
    Position.add cursor p

  method tile_size = tile_size

end


  
