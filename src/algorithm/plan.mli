(* $Id: plan.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* plan *)
type t = Loc.act list

type cost = t -> float           (* plan cost *)
type next = unit -> t            (* empty is end of plans *)

type mkiter = cost -> float (* budget left *) -> next

