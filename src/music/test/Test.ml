(**
   Some testing facilities
*)

open Time
open Music
open DList
open TPTM

let dummy_event_plus (a, b) (pitch, velocity) =
  note (Time.fromPair (a, b)) (new Music.param pitch velocity)

let dummy_simple_event (a, pitch, octave, vel) =
  dummy_event_plus (a, 1) ((pitch, octave), vel)

let dummy_delay : int -> TPTM.t = fun dur ->
  delay (Time.fromInt dur)

(**
   notes : [(int, Music.pitch)], duration and pitch.
*) 
let sequence notes =
  let aggregate tile note =
    tile % (TPTM.make_withDelay (dummy_simple_event note))
  in
  List.fold_left aggregate TPTM.zero notes

let random_note () =
  match (Random.int 6) with
    | 0 -> C | 1 -> Ds | 2 -> F | 3 -> Fs | 4 -> G | 5 -> As | 6 -> C
    | _ -> failwith "Not a note"

let incr_note n = match n with
  | A -> B | B -> C | C -> D | D -> E | E -> F | F -> G | G -> A
  | _ -> failwith "Get lost this is just a test."

let blues_bass note =
  [(1, note, 3, 63) ; (2, note, 3, 0); (1, note, 3, 63) ;
  (1, incr_note note, 3, 63) ; (2, note, 3, 0); (1, incr_note note, 3, 63)]

let blues_ending note =
  blues_bass note @ blues_bass note

let blues_grid =
  sequence
    (blues_bass C @
    blues_bass C @
    blues_bass C @
    blues_bass C @
    blues_bass F @
    blues_bass F @
    blues_bass C @
    blues_bass C @
    blues_bass G @
    blues_bass F @
    blues_ending C)

let min a b = if a < b then a else b

let () =
  Random.self_init ();
  let l1 = ref [] and l2 = ref [] in
  let i = ref 0 in
  while !i < (12*4*2) do
    let len = min (Random.int 2 + 1) (12*4*2 - (!i)) in
    i := !i + len;
    let mute = 64 + (Random.int 4) * (64 / 3) in
    (*Printf.printf "%d\n%!" mute;*)
    l1 := (len, random_note (), 3 + (Random.int 2), mute) :: (!l1)
  done;
  i := 0;
  while !i < (12*4*2) do
    let len = min (Random.int 2 + 1) (12*4*2 - (!i)) in
    i := !i + len;
    let mute = 64 + (Random.int 4) * (64 / 3) in
    l2 := (len, random_note (), 3 + (Random.int 2), mute) :: (!l2)
  done;
  let s1 = sequence (!l1) in
  let s2 = sequence (!l2) in
  let s = fork (fork s1 s2) blues_grid in
  (*let s = blues_grid in*)
  TPTM.play ~tempo:(Num.Int 2) s

