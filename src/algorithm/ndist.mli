(* $Id: ndist.mli 985 2010-06-28 17:28:30Z tolpin $ *)

type t = float*float (* exposed for pattern matching *)

val make: float -> float -> (float*float) (* constructor *)

val mean: t -> float                   (* accessors *)
val variance: t -> float

(* completely unknown distribution *)
val unknown: t
val known: t -> bool

(* probability *)
val pdf: t -> float -> float (* p)robability d)ensity *)
val cdf: t -> float -> float (* c)umulative d)istribution *)

(* random sampling *)
val sample: t -> float
