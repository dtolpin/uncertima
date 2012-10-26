(* $Id: simd.ml 989 2010-06-29 05:59:00Z tolpin $ *)

(* command-line driver for the simulator daemon *)

module P = Pdefinition
module O = Object

open Remote
open Error

let debug = Debug.info "simd"

let main () = 
  (* variable modifiable by options *)
  let replimit = ref (None:int option) 
  and novar = ref false in
    
  let usage = (Printf.sprintf "Usage: %s <options> problem object <server options>\nOptions are:" Sys.argv.(0))
  and speclist =  
    [ "-d", Arg.Set Conf.debug, "debug";
      "-r", Arg.Int (fun r -> replimit := Some r), "limit on repetition of the same observation";
      "-novar", Arg.Set novar, "don't send variance" ] in

  let misuse message retcode = 
    ( Printf.eprintf "Error: %s\n" message 
    ; Arg.usage speclist usage
    ; exit retcode )
  in
    ( try
        ( Arg.parse speclist
          (fun _ ->
             (* two obligatory arguments: problem and object, followed by server options *)
             if (Array.length Sys.argv) < !Arg.current+2
             then misuse "wrong number of arguments" 1 
             else
               let _ = debug "problem and object" in

               let pdefname = Sys.argv.(!Arg.current)
               and objfname = Sys.argv.(!Arg.current+1) in

               let pdef = Pops.parse pdefname in
               let objt = O.simulator pdef (O.from_file objfname) !replimit in
               let crdtab = Pops.tabulate pdef.P.p_coords in

               let to_act = function 
                   (`Struct a) ->
                     (to_int (List.assoc "observation" a),
                      let location = 
                        List.map
                          (function
                               (`Struct c) ->
                                 (try
                                   (to_string (List.assoc "cname" c), to_float (List.assoc "cvalue" c))
                                 with Not_found -> failwith "simd: cname, cvalue expected")
                             | _ -> failwith "simd: coordinate expected")
                          (to_list (List.assoc "location" a))
                      in (List.map (fun { P.c_name = cname } ->
                                      List.assoc
                                        (List.assoc cname location)
                                        (List.assoc cname crdtab))
                            pdef.P.p_coords))
                 | _ -> failwith "simd: action expected"
               and of_evi e = `Array (List.map
                                        (function (iftr, (mean, var)) ->
                                           `Struct
                                             (let mean = [("fname",`String (List.nth pdef.P.p_featrs iftr)); 
                                                          ("fmean", `Double mean) ]
                                              in if !novar then mean else ("fvar", `Double var)::mean)) e)

               in ( Printf.printf "problem:  %s\nobject: %s\n%!" pdefname objfname
                   ; Arg.current := !Arg.current+1 ; Sys.argv.(!Arg.current) <- Sys.argv.(0)
                  ; let server = new XmlRpcServer.netplex () in
                    ( server#register "uncertima.probe"
                      ~help: "Check availability of observation"
                      ~signatures: [[`Boolean; `Struct]]
                      (function [a] -> `Boolean (objt.O.probe (to_act a))
                         | _ -> XmlRpcServer.invalid_params ())
                    ; server#register "uncertima.observe" 
                      ~help: "Obtain observation at location"
                      ~signatures: [[`Array; `Struct]]
                      (function [a] -> of_evi (objt.O.observe (to_act a))
                         | _ -> XmlRpcServer.invalid_params ())
                    ; server#run () )
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

