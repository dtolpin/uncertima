(* $Id: myopic.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition
module O = Object

(* all myopic operations are order of space of the model
   -- the whole list can be kept in memory *)

let mkiter pdef objt cost budget =
  let plans =
    let points =             
      List.fold_right           (* recursive list comprehension *)
        (fun hs ts ->                                                             (* recursively *)
           List.concat                                                            (* concatenate *) 
             (List.map                                                                (* results *)
                (fun t -> (List.map (fun (_,i) -> i::t)                         (* of prepending *)
                             (Pops.enumerate hs)))                          (* each of the heads *)
                ts))                                         (* to each of the accumulated tails *)
        (List.map (fun c -> c.P.c_range) pdef.P.p_coords)           (* for each coordinate range *)
        [[]]                                                         (* starting with empty tail *)
    in List.concat (* every single observation in every point *)
         (List.map
            (fun (iobs, _) ->                                                            (* mate *)
               (List.map (fun point -> [(iobs, point)]) points))                   (* each point *)
            pdef.P.p_obsrvs)                                            (* with each observation *)
  and rest = ref ([]: Plan.t list)

  in ( rest := plans                                                 (* initialize with all plans *)
     ; let rec next () =
         match !rest with                    
             [] -> []
           | (h::t) ->                                           (* return next plan on each step *)
               ( rest := t
               ; if (List.for_all objt.O.probe h)      (* skipping too expensive or expired plans *)
                 && (cost h) <= budget then h else next () )
     in next )

let make metamake pdef modl spac objt = 
  let algo = metamake (mkiter pdef objt) pdef modl spac objt
  in { algo with Algo.name = algo.Algo.name ^ " myopic" }
