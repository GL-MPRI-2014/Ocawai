(**
   Music module
*)

type time = Time.t

type 'a t = Note of time * 'a

type velocity = int
type octave = int
type pitchClass  = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
		   | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
		   | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
		   | Bf | Ass | B | Bs | Bss
type pitch = pitchClass * octave

type param = pitch * velocity
    
type event = param t

let getDur : 'a t -> time = function
  | Note(dur, _) -> dur

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

let rec printf : Format.formatter -> event t -> unit = fun fmt ->
  function
  | Note(dur, param) ->
    Format.fprintf fmt "@[<1>Note(@,%a,@ %a@]" Time.printf dur printParam param

and print_param : Format.formatter -> param -> unit = fun fmt ->
  function
  | (pitch, velocity) ->
    Format.fprintf fmt "@[<1>(pitch =@ %a,@ velocity =@ %d@,)@]"
      print_pitch pitch 
