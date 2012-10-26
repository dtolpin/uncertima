(* $Id: loc.mli 985 2010-06-28 17:28:30Z tolpin $ *)

type point = int list            (* point in space *)  
type section = int option list   (* space section *)
type subspace = section          (* subspace is dual to section: definite coordinates
                                    are enumerated over their range *)
type obs = point                 (* observation location is a point *)
type opt = section               (* optimization location is a section *)

type act = int * obs             (* observation action in a location is product of
				    observation type and observation location *)

(* specialize section by subsection *)
val specialize: section -> section -> section

(* enumerate all sections in intersection of the section and the subspace *)
val comprehend: section -> subspace -> section list

(* select optimization locations affected by observation locations *)
val pierce: section list -> point list -> section list

(* section containing just this point *)
val thesection: point -> section
