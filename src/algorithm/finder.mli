(* $Id: finder.mli 985 2010-06-28 17:28:30Z tolpin $ *)

type mkbudget = float -> float  (* budget surplus before the algorithm
                                     -> budget for the algorithm *)
val run:
  (Algo.make * mkbudget) list  (* algorithms and their budgets *)
  -> Pdefinition.t             (* problem definition *)
  -> Model.t                   (* model *)
  -> Space.t                   (* metric space *)
  -> Object.t                  (* object or simulator *)
  -> Log.t                     (* logger *)
  -> float                     (* budget *)
  -> ((Loc.opt*float)*float)   (* (alpha=(location,utility), surplus) *)

(* some budget functions *)
val greedy_budget: mkbudget                   (* can eat all of the surplus *)
val fixed_budget: float -> mkbudget           (* at most the fixed budget *)
val fixed_surplus: float -> mkbudget          (* the budget does not cross the surplus threshold *)
val budget_fraction: float -> mkbudget        (* the budget is the fraction of the total budget *)
