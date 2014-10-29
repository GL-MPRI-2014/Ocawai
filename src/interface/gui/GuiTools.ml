open OcsfmlGraphics

open Manager

type alignment = Center | Left | Right
type quantity = Pix of int | Pt of float

let to_pixels = function
  | Pix i -> i
  | Pt p -> int_of_float (0.72 *. p)

let rec rect_print (target : #OcsfmlGraphics.render_target)
  string font color size interline alignment rectangle =

  let character_size = to_pixels size in
  let interline_size = to_pixels interline in

  let text = new text
    ~string
    ~font
    ~color
    ~character_size
  () in

  let text_bounds = text#get_global_bounds in

  try

    (* If we cannot draw it in height, we're done *)
    if text_bounds.height > rectangle.height then raise Exit ;

    (* If the text is too long for a line *)
    if text_bounds.width > rectangle.width then begin

      (* We compute the line to be drawn and draw the rest recursively *)
      let words = Str.split (Str.regexp " ") text#get_string in

      (* We clear the text object *)
      text#set_string "";

      (* We create a function to check if s is too long *)
      (* Sets the text if not too long and if [reset] is false *)
      let set_too_long reset s =
        let old_s = text#get_string in
        text#set_string s;
        let text_bounds = text#get_global_bounds in
        let b = text_bounds.width > rectangle.width in
        if (reset || b) then text#set_string old_s;
        b
      in
      (* Pure comparison *)
      let toolong = set_too_long true in

      (* Enlarges the string while it fits *)
      let rec fit strel sep =
        let s = text#get_string in
        if not (set_too_long false (s ^ sep ^ (List.hd strel))) then
          fit (List.tl strel) sep
        else String.concat sep strel
      in

      (* We have to treat the case where the first word is already too long *)
      let char_list str =
        let l = ref [] in
        String.iter (fun c -> l := (String.make 1 c) :: !l) str;
        List.rev !l
      in

      (* Computes the remaining text and sets the current line *)
      let remaining = match words with
        | [] -> failwith "inconsistent test"
        | w::r when toolong w -> String.concat " " ((fit (char_list w) "") :: r)
        | strel -> fit strel " "
      in

      (* Dealing with the interline *)
      let delta = float_of_int (character_size + interline_size) in
      let new_rect = {
        left = rectangle.left ;
        top = rectangle.top +. delta ;
        width = rectangle.width ;
        height = rectangle.height -. delta
      } in

      (* We are now ready to call the function recursively *)
      rect_print target remaining font color size interline alignment new_rect
    end ;

    (* In case things were modified we recompute the bounds *)
    let text_bounds = text#get_global_bounds in

    (* The line we're about to draw is aligned through offset *)
    let ox = match alignment with
      | Center -> (rectangle.width -. text_bounds.width) /. 2.
      | Left   -> 0.
      | Right  -> rectangle.width -. text_bounds.width
    in
    (* text#set_origin ox 0.; *)

    text#set_position (rectangle.left +. ox) rectangle.top;

    target#draw text

  with Exit -> ()
