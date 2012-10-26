(* $Id: hypothesis.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* prior hypothesis: speculative evidence in some observation locations*)

(* apply a hypothesis to the model *)
val apply: Pdefinition.t -> Model.t -> string (* data file *) -> unit

