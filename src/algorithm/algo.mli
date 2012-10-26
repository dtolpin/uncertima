(* $Id: algo.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* type definitions for algorithm abstraction *)

type t = { (* algorithm name, for monitoring *)
           name: string;

           (* given the budget, select 0 or more locations to observe *)
           select: float (* T *) -> Plan.t;

           (* observe selected locations, and return the budget left *)
           commit: float (* T *) -> Plan.t -> float (* T' *);

           (* opinion state -- opinion is utility and its components *)
           opinions: unit -> (Loc.opt * float list) list;
           
           (* currently best location and its utility *)
           alpha: unit -> Loc.opt * float }

type make = Pdefinition.t (* problem definition *)
         -> Model.t       (* model, updated by the algorithm *)
         -> Space.t       (* coordinate space *)
         -> Object.t      (* object, accessed (and implicitly updated) *)
         -> t

(* functions shared by many algorithms *)
module Lib: sig 
  type t = { (* expected utility *)
             eu: Loc.opt -> float;

             (* expected binding, for reporting and analysis *)
             eb: int -> Loc.opt -> float;

             (* plan cost *)
             cost: Plan.t -> float;

             (* currently best location and its utility *)
             alpha: Loc.opt list   (* optimization locations *)
             -> float list                (* their utilities *)
             -> (Loc.opt * float);   (* location and utility *)         

             (* benefit *)
             bene: Plan.t                         (* plan *)
             -> Loc.opt list    (* optimization locations *)
             -> float list             (* prior utilities *)
             -> float;
  
             (* observe locations and update the model *)
             obsupd: Plan.t
             -> Loc.opt list        (* optimization locations *)
             -> float list                 (* prior utilities *)
             -> float list }           (* posterior utilities *) 


  val make:  Pdefinition.t -> Model.t -> Space.t -> Object.t -> t

end
