(* $Id: model.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* coordinates and features are numbered in the declaration order *)

type t = { (* fetch feature belief *)
           fetch: Loc.section        (* coordinates *)
           -> int                    (* feature *)
           -> Ndist.t list;          (* current belief *)

           (* update feature with evidence *)
           update: Loc.obs           (* coordinates *)
           -> int                    (* feature *)
           -> Ndist.t                (* evidence *)
           -> bool                   (* real or simulated update? *)
           -> unit;

           (* propagate the updates through the dependencies *)
           propagate: unit -> unit;

           (* commit the update *)
           commit: unit -> unit;

           (* rollback to previous committed update *)
           rollback: unit -> unit;

           (* true iff any of the locations was affected by the last update *)
           modified: Loc.opt -> bool;

           (* modification rate in the current state; for debugging and analysis *)
           modification_rate: unit -> float;

           (* iterate over all points and apply a function in each *)
           iter: (Loc.obs -> Ndist.t list -> unit) -> unit }

val make: Pdefinition.t -> t
