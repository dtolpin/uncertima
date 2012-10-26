(* $Id: markov.mli 985 2010-06-28 17:28:30Z tolpin $ *)

(* markov random field *)

type 'a t = { (* notify of evidence *)
              notify: 'a -> Ndist.t -> unit;
  
              (* propagate beliefs *)
              propagate: ('a -> Ndist.t -> bool) (* update belief, return true iff updated *)
              -> 'a list (* modification centers *)
              -> unit }

val make: 'a list          (* all nodes *)
    -> ('a -> 'a list)     (* node neighbors *)
    -> ('a -> 'a -> float) (* internode noise (\sigma^2) *)
    -> 'a t
  
