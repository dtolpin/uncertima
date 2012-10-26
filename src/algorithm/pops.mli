(* $Id $*)

(* problem definition processing *)

(* parse problem definition from file -- convenience wrapper *)
val parse: string -> Pdefinition.t

(* number list elements, turning each element into sequentially
   numbered pair -- good for looking up enumerated values *)
val enumerate: 'a list -> ('a*int) list

(* tabulate coordinates *)
val tabulate: Pdefinition.coordinate list -> (string*(float*int) list) list
