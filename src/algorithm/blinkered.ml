(* $Id: blinkered.ml 985 2010-06-28 17:28:30Z tolpin $ *)

let make metamake pdef modl spac objt = 
  let mkiter cost budget = 
    (* use myopic iterator but pump the plans within the budget *)
    let myo_next = Myopic.mkiter pdef objt cost budget in

    (* next_plan and plan_cost are plan and cost of the next plan to consider *)
    let next_plan = ref ([]:Plan.t) and plan_cost = ref max_float in
      (* on startup, plan_cost > budget; myo_next is called immediately *)

    let rec next () =                 
      if !plan_cost <= budget then (* the plan is within the budget, return it and ... *)
        let plan = !next_plan                     (* ... extend for the next iteration *)
        in ( next_plan := !next_plan @ !next_plan
           ; plan_cost := cost !next_plan
           ; plan )
      else     (* the plan is longer than the budget allows, retrieve next myopic plan *)
        ( next_plan := myo_next ()
        ; plan_cost := cost !next_plan
        ; match !next_plan with
            [] -> []  (* end of plans; next call to next would raise exception *)
          | _ -> next () )
    in next in
  let algo = metamake mkiter pdef modl spac objt
  in { algo with Algo.name = algo.Algo.name ^ " blinkered" }
