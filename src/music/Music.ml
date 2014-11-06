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

