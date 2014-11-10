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

and print_pitchClass : Format.formatter -> pitchClass -> unit = fun fmt ->
  function
  | Cff ->
    Format.fprintf fmt "@[%s@]" "Cff"
  | Cf -> 
    Format.fprint fmt "@[%s@]" "Cf" 
  | C -> 
    Format.fprint fmt "@[%s@]" "C"
  | Dff -> 
    Format.fprint fmt "@[%s@]" "Dff" 
  | Cs -> 
    Format.fprint fmt "@[%s@]" "Cs" 
  | Df -> 
    Format.fprint fmt "@[%s@]" "Df" 
  | Css -> 
    Format.fprint fmt "@[%s@]" "Css" 
  | D -> 
    Format.fprint fmt "@[%s@]" "D" 
  | Eff -> 
    Format.fprint fmt "@[%s@]" "Eff" 
  | Ds -> 
    Format.fprint fmt "@[%s@]" "Ds" 
  | Ef -> 
    Format.fprint fmt "@[%s@]" "Ef" 
  | Fff -> 
    Format.fprint fmt "@[%s@]" "Fff" 
  | Dss -> 
    Format.fprint fmt "@[%s@]" "Dss" 
  | E -> 
    Format.fprint fmt "@[%s@]" "E"
  | Ff -> 
    Format.fprint fmt "@[%s@]" "Ff" 
  | Es -> 
    Format.fprint fmt "@[%s@]" "Es" 
  | F -> 
    Format.fprint fmt "@[%s@]" "F"
  | Gff -> 
    Format.fprint fmt "@[%s@]" "Gff" 
  | Ess -> 
    Format.fprint fmt "@[%s@]" "Ess" 
  | Fs -> 
    Format.fprint fmt "@[%s@]" "Fs" 
  | Gf -> 
    Format.fprint fmt "@[%s@]" "Gf" 
  | Fss -> 
    Format.fprint fmt "@[%s@]" "Fss" 
  | G -> 
    Format.fprint fmt "@[%s@]" "G"
  | Aff -> 
    Format.fprint fmt "@[%s@]" "Aff" 
  | Gs -> 
    Format.fprint fmt "@[%s@]" "Gs" 
  | Af -> 
    Format.fprint fmt "@[%s@]" "Af" 
  | Gss -> 
    Format.fprint fmt "@[%s@]" "Gss" 
  | A -> 
    Format.fprint fmt "@[%s@]" "A"
  | Bff -> 
    Format.fprint fmt "@[%s@]" "Bff" 
  | As -> 
    Format.fprint fmt "@[%s@]" "As" 
  | Bf -> 
    Format.fprint fmt "@[%s@]" "Bf" 
  | Ass -> 
    Format.fprint fmt "@[%s@]" "Ass" 
  | B -> 
    Format.fprint fmt "@[%s@]" "B"
  | Bs -> 
    Format.fprint fmt "@[%s@]" "Bs" 
  | Bss -> 
    Format.fprint fmt "@[%s@]" "Bss" 
