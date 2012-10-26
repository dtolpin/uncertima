(* $Id: algo.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition
module E = Expression
module M = Model
module S = Space
module O = Object

let debug = Debug.info "algo"

(* type definitions for algorithm abstraction *)

type t = { name: string;
           select: float -> Plan.t;
           commit: float -> Plan.t -> float;
           opinions: unit -> (Loc.opt*float list) list;
           alpha: unit -> Loc.opt*float }

type make = Pdefinition.t -> Model.t -> Space.t -> Object.t -> t

(* monte carlo integration of a generator *)
let integrate f eps = 
  let rec loop suma sumb n = 
    let sum = suma +. sumb in
      if n >= !Conf.number_of_integration_points
        || abs_float (suma -. sumb) < sum *. eps
      then sum /. n
      else loop (suma +. f ()) (sumb +. f ()) (n +. 2.0)
  in loop 0.0 0.0 0.0

module Lib = struct
  type t = { eu: Loc.opt -> float;
             eb: int -> Loc.opt -> float;
             cost: Plan.t -> float;
             alpha: Loc.opt list -> float list -> (Loc.opt * float);
             bene: Plan.t -> Loc.opt list -> float list -> float;
             obsupd: Plan.t -> Loc.opt list -> float list -> float list }

  let make pdef modl spac objt = 
    let obsenv lobs =
      { E.null_environment with 
          E.env_coord = spac.S.coord lobs;
          E.env_cdiff = spac.S.cdiff lobs } in
      
    (* expressions for merit and auxiliary bindings are computed for every point;
       pre-compile them *)
    let meritexp = E.compile pdef pdef.P.p_find.P.m_expr
    and bindexps = Array.of_list (List.map
                                    (fun b -> E.compile pdef b.P.b_expr)
                                    pdef.P.p_bindgs) in 

    let optenv lopt =
      let rec env =
        { E.null_environment with
            E.env_coord = spac.S.coord (spac.S.someobs lopt);
            E.env_bindg = (fun i -> (bindexps.(i) env));
            E.env_section =
            (fun ftr sec ->
               List.map Ndist.sample 
                 (modl.M.fetch (Loc.specialize lopt sec) ftr)) } 
      in env in

    (* the time is spent in this function;
       better numerical integration will improve the performance *)
    let expected exp lopt =
      let env = optenv lopt
      in integrate (fun () -> exp env) !Conf.integration_precision in

    let eu = expected meritexp
    and eb i = expected bindexps.(i) in

    (* movement and observation cost epxressions are precompiled *)
    let moveexp = E.compile pdef pdef.P.p_cost
    and obsexps =
      List.map (fun (iobs, o) -> (iobs, E.compile pdef o.P.o_cost))
        pdef.P.p_obsrvs in

    (* to compute the cost, go by the plan path; then return to the origin *)
    let cost plan = 
      let orig = spac.S.atobs () in
      let rec loop plan sum =
        match plan with
            [] -> ( spac.S.toobs orig; sum )
          | (iobs,lobs)::plan ->
              let env = obsenv lobs in
              let c = (moveexp env) +. ((List.assoc iobs obsexps) env)
              in ( spac.S.toobs lobs; loop plan (sum +. c) )
      in loop plan 0.0 in

    (* random optimisation location for the guard value of alpha;
       make it valid so that if utility is not computable,
       the algorith still chooses a location, and not throws
       an exception *)
    let some_lopt = spac.S.someopt () in
      
    let alpha =
      List.fold_left2 
        (fun ((lmax,umax) as lumax) l u ->
           if u > umax then (l,u) else lumax)
        (some_lopt, -.max_float) in

    (* compute posterior utilities;
       when a location was not modified by the last update,
       just copy the prior utility and save on the integration time *)
    let posteus = 
      List.map2 
        (fun lopt u ->
           if modl.M.modified lopt then eu lopt else u) in

    (* index the features *)
    let features = Pops.enumerate pdef.P.p_featrs in

    let bene plan lopts us = 
      let (l_alpha,_) = alpha lopts us in
      let g () =
        let rec emulate plan =
          match plan with
              [] -> ()
            | (iobs,lobs)::plan -> 
                ( (* emulate the observations *)
                  ( let o = List.assoc iobs pdef.P.p_obsrvs
                    and sobs = (Loc.thesection lobs)
                    in List.iter2
                         (fun feature ve ->
                            let ftr = List.assoc feature features in
                            let dist = List.hd (modl.M.fetch sobs ftr) in
                            let me = Ndist.sample dist
                            in modl.M.update lobs ftr (Ndist.make me ve) false)
                         o.P.o_featrs o.P.o_accurs)
                  ; emulate plan )
        in ( emulate plan
             (* the model has been modified by emulated observations *)
           ; let (_, u_alpha_post) = alpha lopts (posteus lopts us)
             and u_alpha = eu l_alpha
             in ( modl.M.rollback ()
                ; u_alpha_post-.u_alpha ) )
      in (* nested imprecise integration: the relative error doubles *)
        ( Stat.benefit_integrations := !Stat.benefit_integrations + 1
	; integrate g (2.0 *. !Conf.integration_precision) ) in

    let obsupd plan lopts us =
      ( List.iter
          (fun ((iobs,lobs) as o) ->
             spac.S.toobs lobs
             ; List.iter
               (fun (ftr,e) -> modl.M.update lobs ftr e true)
               (objt.O.observe o))
          plan
      ; modl.M.propagate () 
      ; debug (Printf.sprintf "mod=%g" (modl.M.modification_rate ()))
      ; let us = posteus lopts us
      in ( modl.M.commit ()
         ; us ) )

    in { eu = eu;
         eb = eb;
         cost = cost;
         alpha = alpha;
         bene = bene;
         obsupd = obsupd }
end
