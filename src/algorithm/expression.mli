(* $Id: expression.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* expressions are computed for two purposes:

   1) action cost
   2) utility function

   computing action cost is relatively inexpensive. computing utility
   must be fast -- it occurs often, and the formula can be complex. *)

type environment =
    { env_coord: int -> float;    (* index -> coordinate *)
      env_cdiff: int -> float;    (* index -> coordinate difference *)
      env_bindg: int -> float;    (* index -> computed binding *)
      env_section: int            (* variable index *)
      -> Loc.section              (* section *)
      -> float list }             (* return vector value *)

(* an environment that does not contain values *)

val null_environment: environment
  
type t = environment -> float

(* the expression is compiled from the expression definition given the name
   translation table and the coordinate dimensions. coordinate dimensions
   are needed to compute sections. aggregates, unary and binary functions
   are defined in the environment. *)

val compile:
  Pdefinition.t                     (* environment -- coordinates, features, bindings *)
  -> Pdefinition.expression         (* expression source *)
  -> t                              (* compiled expression *)

