(* $Id: finder.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition
module M = Model
module O = Object
module A = Algo
module L = Log

let debug = Debug.info "finder"

type mkbudget = float -> float

let run algos pdef modl spac objt log budget = 

  (* single algorithm *)
  let runalgo (_, budget) (mkalgo, mkbudget) = 

    let _ = debug "runalgo" in

    let algobudget = mkbudget budget
    and { A.name = algoname;
          A.select = select;
          A.commit = commit;
          A.opinions = opinions;
          A.alpha = alpha } = mkalgo pdef modl spac objt in

    (* the greedy loop: choose, commit, stop *)
    let rec loop b =
      match select b with
          [] -> (* store beliefs and opinions after each algorithm *)
            ( log.L.beliefs modl 
            ; log.L.opinions (opinions ())
              (* retrieve alpha and budget surplus *)
            ; (alpha (), budget -. algobudget +. b) )
        | plan -> 
            let prea = alpha () in
            let b = commit b plan in
            let a = alpha () 
            in ( log.L.observations prea plan a
               ; debug (Printf.sprintf "budget=%g" b)
               ; loop b )
    in ( log.L.algorithm algoname
       ; loop algobudget )

  in ( log.L.header ()
     ; List.fold_left (* fold algorithms to (alpha, surplus) *)
         runalgo
         ((List.map (fun _ -> None) pdef.P.p_coords, nan), (* unknown alpha *)
          budget)
         algos )

let greedy_budget s = s
let fixed_budget b s = min b s
let fixed_surplus s' s = max (s-.s') 0.
let budget_fraction f s = f*.s
