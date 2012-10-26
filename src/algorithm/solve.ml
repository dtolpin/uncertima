(* $Id: solve.ml 992 2010-06-30 15:58:28Z tolpin $ *)

(* command-line driver for the finder *)

module P = Pdefinition
module O = Object

open Init
open Error

let debug = Debug.info "solve"

(* default algorithm is the only stage if no algorithm was specified *)
let default_algorithm = "myopic"

(* default budget function is to always consume as much
   of the budget as feasible *)
let default_budget_function = "greedy"

(* table of algorithms and their make functions, for Arg.Symbol *)
let algorithms = 
  [ "montecarlo", Myopic.make Montecarlo.make;
    "myopic", Myopic.make Planhead.make;
    "blinkered", Blinkered.make Planhead.make;
    "Myopic", Myopic.make Metahead.make;
    "Blinkered", Blinkered.make Metahead.make ]

(* table of budget functions;  the functions have different 
   signatures, thus the wrappers *)
let budget_functions =
  [ "greedy", (fun _ -> Finder.greedy_budget);
    "fixed", (fun b -> Finder.fixed_budget b);
    "surpus", (fun b -> Finder.fixed_surplus b);
    "fraction", (fun b -> Finder.budget_fraction b)]

let main () = 
  (* variable modifiable by options *)
  let total_budget = ref max_float
  and algorithm = ref default_algorithm
  and budget = ref max_float
  and budget_function = ref default_budget_function
  and logfname = ref ""
  and noprobe = ref false
  and replimit = ref (None:int option)
  and simulate = ref false 
  and stat = ref false in

  (* list of stages (algorithm+budget) *)
  let stages = ref ([]:(Algo.make*Finder.mkbudget) list) in
  (* - stage parameters are either inherited from a previous stage
       or modified after the algorithm name;
     - an algorithm defines the stage, and the stage is pushed when
       the next stage's algorithm is specified *)
  let push_stage () = 
    stages := 
      (List.assoc !algorithm algorithms,
       (List.assoc !budget_function budget_functions) !budget)::!stages in
    
  let usage = (Printf.sprintf
                 "Usage: %s <options> problem object <xml-rpc server options>\nOptions are:"
                 Sys.argv.(0))
  and speclist =  
    [ "-a", Arg.Symbol 
        ((List.map fst algorithms),
         fun aname ->  push_stage (); algorithm := aname), " algorithm";
      "-B", Arg.Set_float total_budget, "total budget";
      "-b", Arg.Set_float budget, "algorithm's budget";
      "-c", Arg.Set_float Conf.benefit_cost, "cost of computing a benefit";
      "-d", Arg.Set Conf.debug, "debug";
      "-f", Arg.Symbol
        ((List.map fst budget_functions),
         fun bf -> budget_function := bf), " budget function";
      "-l", Arg.Set_string logfname, "log file prefix";
      "-i", Arg.Set_float Conf.number_of_integration_points,
           "number of integration points";
      "-j", Arg.Set_int Conf.number_of_propagations,
           "number of belief propagation passes";
      "-noprobe", Arg.Set noprobe,
           "measurements are infinitely repeatable";
      "-p", Arg.Set_float Conf.integration_precision,
           "integration precision";
      "-q", Arg.Set_float Conf.belief_update_precision,
           "belief update precision";
      "-r", Arg.Int (fun r -> replimit := Some r),
           "limit on repetition of the same observation";
      "-s", Arg.Set simulate, "simulate";
      "-stat", Arg.Set stat, "print statistics" ] in

  let misuse message retcode = 
    ( Printf.eprintf "Error: %s\n" message 
    ; Arg.usage speclist usage
    ; exit retcode )
  in
    ( try
        ( Arg.parse speclist
          (fun _ -> push_stage () ;
             (* two obligatory positional arguments and one optional *)
             if not (List.mem (Array.length Sys.argv) [!Arg.current+2; !Arg.current+3])
             then misuse "wrong number of arguments" 1 
             else
               let _ = debug "problem and object" in
               let pdefname = Sys.argv.(!Arg.current)
               and objfname = Sys.argv.(!Arg.current+1)
               and hypfname =
                 if (Array.length Sys.argv = !Arg.current+3) 
                 then Sys.argv.(!Arg.current+2)
                 else "" in
               let logfname =
                 if !logfname = ""
                 then
                   try
                     String.sub pdefname 0 (String.rindex pdefname '.')
                   with Not_found -> pdefname
                 else !logfname in

               let stages =
                 match !stages with
                     [stage] -> !stages       (* the only one is the default *)
                   | _ -> List.tl (List.rev !stages) in (* first is sentinel *)
               let pdef = Pops.parse pdefname in
               let modl = Model.make pdef
               and spac = Space.make pdef
               and objt =
                 if !simulate
                 then O.simulator pdef (O.from_file objfname) !replimit
                 else 
                   try Remote.make ~noprobe:!noprobe pdef objfname
                   with XmlRpc.Error (_, message) ->
                     failwith (Printf.sprintf "cannot connect to remote object %s: %s"
                                 objfname message)
               and log = Log.make pdef logfname
               in ( Printf.printf "problem:  %s\nobject: %s\n" pdefname objfname
                  ; if hypfname <> "" then Printf.printf "hypothesis: %s\n" hypfname
                  ; Printf.printf "logs: %s.*\n%!" logfname
                  ; if hypfname <> "" then Hypothesis.apply pdef modl hypfname
                  ; let ((lopt,u),surplus) =
                    Finder.run stages pdef modl spac objt log !total_budget in
                    (* report chosen parameters, utility and budget surplus *)
                  let coord = spac.Space.coord (spac.Space.someobs lopt)
                  in ( Printf.printf "solution: "
                     ; List.iter2
                       (fun ({P.c_name = name}, i) value ->
                          match value with
                              Some _ -> Printf.printf "%s=%g " name (coord i)
                            | None -> ()) (* not an optimization parameter *)
                       (Pops.enumerate pdef.P.p_coords) lopt
                     ; Printf.printf "=> %s=%g / surplus=%g"
                       pdef.P.p_find.P.m_name u surplus 
                     ; if !stat then
		       Printf.printf " [time=%g bene=%d/%d]"
			 (Unix.times ()).Unix.tms_utime
			 !Stat.benefit_evaluations
			 !Stat.benefit_integrations
                     ; Printf.printf "\n%!" )
                    ; exit 0 ))
          usage
        ; misuse "missing arguments" 1 )
      with 
          P.ParseError (e, pos) -> definition_error e (Some pos)
        | P.CheckError e -> definition_error e None
        | O.DataError e -> data_error e
        | Sys_error e
        | Failure e -> error e )
    ; exit 2
          

;; 

if not !Sys.interactive then
  ( Init.seed_random_source ()
  ; main () )
