(**
   Music module
*)

type time = Time.t

type 'a t = Note of time * 'a

type velocity = int (** To-Do : is there a special mm.int
			type for MIDI values in mm ? *) 
type octave = int
type pitchClass  = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds 
		   | Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
		   | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As 
		   | Bf | Ass | B | Bs | Bss
type pitch = pitchClass * octave

type param = pitch * velocity
    
type event = param t

let return : time -> param -> event = fun dur (pitch, velocity) ->
  if (velocity <= 127 && velocity >= 0) then
    Note(dur, (pitch, velocity))
  else failwith "Incompatible value for velocity"

let getDur : 'a t -> time = function
  | Note(dur, _) -> dur

(** {2 Testing functions} *)

(** {3 Pretty-printing} *)

let rec printf : Format.formatter -> event t -> unit = fun fmt ->
  function
  | Note(dur, param) ->
    Format.fprintf fmt "@[<1>Note(@,%a,@ %a@]" Time.printf dur print_param param

and print_param : Format.formatter -> param -> unit = fun fmt ->
  function
  | (pitch, velocity) ->
    Format.fprintf fmt "@[<1>(pitch =@ %a,@ velocity =@ %d@,)@]"
      print_pitch pitch velocity

and print_pitch : Format.formatter -> pitch -> unit = fun fmt ->
  function
  | (pitchClass, octave) ->
    Format.fprintf fmt "@[<1>%a%d@]"
      print_pitchClass pitchClass octave

and print_pitchClass : Format.formatter -> pitchClass -> unit = fun fmt pitch_class ->
  let pitchClass_name = match pitch_class with
    | Cff -> "Cff" | Cf -> "Cf"   | C -> "C"
    | Dff -> "Dff" | Cs -> "Cs"   | Df -> "Df" 
    | Css -> "Css" | D -> "D"     | Eff -> "Eff" 
    | Ds -> "Ds"   | Ef -> "Ef"   | Fff -> "Fff" 
    | Dss -> "Dss" | E -> "E"     | Ff -> "Ff" 
    | Es -> "Es"   | F -> "F"     | Gff -> "Gff" 
    | Ess -> "Ess" | Fs -> "Fs"   | Gf -> "Gf" 
    | Fss -> "Fss" | G -> "G"     | Aff -> "Aff" 
    | Gs -> "Gs"   | Af -> "Af"   | Gss -> "Gss" 
    | A -> "A"     | Bff -> "Bff" | As -> "As" 
    | Bf -> "Bf"   | Ass -> "Ass" | B -> "B"
    | Bs -> "Bs"   | Bss -> "Bss" 
  in Format.fprintf fmt "@[%s@]"
