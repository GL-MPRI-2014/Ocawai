open OcsfmlGraphics
open OcsfmlWindow
open Utils

type t = {
    texture: texture;
    up        : int rect;
    up_right  : int rect;
    right     : int rect;
    down_right: int rect;
    down      : int rect;
    down_left : int rect;
    left      : int rect;
    up_left   : int rect;
    center    : int rect 
}


let create tex ~up ~up_right ~right ~down_right 
               ~down ~down_left ~left ~up_left ~center = 
	{texture = tex; up; up_right; right; down_right; 
                   down; down_left; left; up_left; center}

class sliced_texture filename ~upcut ~downcut ~rightcut ~leftcut = 
  let tex_base = new texture (`File filename) in object(self)

  inherit CustomDrawable.drawable
  
  val sl_texture = 
    tex_base#set_smooth true;
    let (w,h) = tex_base#get_size in 
    let (u,d,r,l) = (upcut, downcut, rightcut, leftcut) in 
    let up_left    = IntRect.create ~position: (0, 0) ~size:(l    , u    ) ()
    and up         = IntRect.create ~position: (l, 0) ~size:(r - l, u    ) ()
    and up_right   = IntRect.create ~position: (r, 0) ~size:(w - r, u    ) ()
    and left       = IntRect.create ~position: (0, u) ~size:(l    , d - u) ()
    and center     = IntRect.create ~position: (l, u) ~size:(r - l, d - u) ()
    and right      = IntRect.create ~position: (r, u) ~size:(w - r, d - u) ()
    and down_left  = IntRect.create ~position: (0, d) ~size:(l    , h - d) ()
    and down       = IntRect.create ~position: (l, d) ~size:(r - l, h - d) ()
    and down_right = IntRect.create ~position: (r, d) ~size:(w - r, h - d) ()
    in create tex_base ~up_left ~up ~up_right ~left ~center 
                       ~right ~down_left ~down ~down_right

  method default_size = tex_base#get_size

  (* rotation/origin/color are unused for now *)
  method draw ~target ?position:(position=(0.,0.)) ?size:(size=(0.,0.)) 
              ?rotation:(rotation=0.) ?origin:(origin=(0.,0.)) 
              ?color:(color=Color.white) ?scale:(scale=(1.,1.)) 
              ?blend_mode:(blend_mode=BlendAlpha) () =
    let (++) = add2D in let (--) = sub2D in
    let rp r = OcsfmlGraphics.(r.left, r.top)     in 
    let rs r = OcsfmlGraphics.(r.width, r.height) in
    let ffst = fun s -> float_of_int (fst s)  in 
    let fsnd = fun s -> float_of_int (snd s)  in 
    let size = iof2D size in
    let position = iof2D position in

    let scalew = (fst scale) *. 
      (ffst (size -- (rs sl_texture.up_right) -- 
      (rs sl_texture.up_left))) /. (ffst (rs sl_texture.up))   in 
    let scaleh = (snd scale) *.
      (fsnd (size -- (rs sl_texture.up_right) -- 
      (rs sl_texture.up_left))) /. (fsnd (rs sl_texture.left)) in 

    let dborder = snd size - (snd (rs sl_texture.down))  in 
    let rborder = fst size - (fst (rs sl_texture.right)) in
    let lborder = fst (rp sl_texture.down) in 
    let uborder = snd (rp sl_texture.right) in 

    let up_sprite    = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((lborder, 0) ++ position))
                          ~scale:(scalew, 1.)
                          ~texture_rect:(sl_texture.up) () in 

    let right_sprite = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((rborder, uborder) ++ position))
                          ~scale:(1., scaleh)
                          ~texture_rect:(sl_texture.right) () in 

    let left_sprite  = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((0, uborder) ++ position))
                          ~scale:(1., scaleh)
                          ~texture_rect:(sl_texture.left) () in

    let down_sprite  = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((lborder, dborder) ++ position))
                          ~scale:(scalew, 1.)
                          ~texture_rect:(sl_texture.down) () in

    let ul_sprite    = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D position)
                          ~scale
                          ~texture_rect:(sl_texture.up_left) () in 

    let ur_sprite    = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((rborder, 0) ++ position))
                          ~scale
                          ~texture_rect:(sl_texture.up_right) () in

    let dr_sprite    = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((rborder, dborder) ++ position))
                          ~scale
                          ~texture_rect:(sl_texture.down_right) () in

    let dl_sprite    = new sprite 
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((0, dborder) ++ position))
                          ~scale
                          ~texture_rect:(sl_texture.down_left) () in

    let c_sprite     = new sprite  
                          ~texture:(sl_texture.texture) 
                          ~position:(foi2D ((lborder, uborder) ++ position))
                          ~scale:(scalew, scaleh)
                          ~texture_rect:(sl_texture.center) () in 

    List.iter (target#draw ~blend_mode) 
      [up_sprite; right_sprite; left_sprite; down_sprite; ul_sprite; 
       ur_sprite; dr_sprite; dl_sprite; c_sprite]

end
