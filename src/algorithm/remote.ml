(* $Id: remote.ml 989 2010-06-29 05:59:00Z tolpin $ *)

module P = Pdefinition
module O = Object

let to_bool = function (`Boolean b) -> b | _ -> failwith "remote: boolean expected"
let to_string = function (`String s) -> s | _ -> failwith "remote: string expected"
let to_int = function (`Int i) -> i | _ -> failwith "remote: int expected"
let to_float = function (`Double d) -> d | _ -> failwith "remote: float expected"
let to_list = function (`Array a) -> a | _ -> failwith "remote: array expected"

let make ?(noprobe=false) pdef url =
  let rpc = new XmlRpc.client url in

  let of_act (iobs, lobs) = 
    `Struct [("observation", `Int iobs);
             ("location",
              `Array (List.map2 (fun c v -> 
                                   `Struct [("cname", `String c.P.c_name);
                                            ("cvalue", `Double (List.nth c.P.c_range v))])
                        pdef.P.p_coords lobs))]
  in let to_evi (iobs, _) =
    let ftridx = O.ftridx pdef 
    and ws = (* bind accuracy estimates to features -- in case variances are not specified *)
      let obs = List.assoc iobs pdef.P.p_obsrvs
      in List.map2 (fun a  b -> (a,b)) obs.P.o_featrs obs.P.o_accurs
    in function (`Array features)
        -> List.map (function (`Struct f) ->
                       ( try 
                           let name = (to_string (List.assoc "fname" f))
                           in (ftridx name,
                               Ndist.make
                                 (to_float (List.assoc "fmean" f))
                                 (try to_float (List.assoc "fvar" f)
                                  with Not_found -> List.assoc name ws))
                         with Not_found -> failwith "remote: feature expected" )
                       | _ -> failwith "remote: expected array of observed features")
          features
      | _ -> failwith "remote: array of feature names and beliefs expected" in
    
  let probe = if noprobe then function _ -> true else
    function a -> to_bool (rpc#call "uncertima.probe" [of_act a])
  and observe a = to_evi a (rpc#call "uncertima.observe" [of_act a])

  in ( ignore (rpc#call "system.listMethods" []) (* ping to check the server *)
     ; { Object.probe = probe
       ; Object.observe = observe } )
