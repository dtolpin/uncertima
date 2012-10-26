(* $Id: conf.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* configuration settings *)

(* maximum number of iterations for monte-carlo integration *)
val number_of_integration_points: float ref

(* convergence precision for monte-carlo integration *)
val integration_precision: float ref

(* maximum number of belief propagation passes *)
val number_of_propagations: int ref

(* belief update sensibility precision *)
val belief_update_precision: float ref

(* cost to compute benefit of a single action *)
val benefit_cost: float ref

(* whether to print debugging output *)
val debug: bool ref

