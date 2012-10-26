(* $Id: space.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition

type t = { atobs: unit -> Loc.obs;
           toobs: Loc.obs -> unit;
           someopt: unit -> Loc.opt;
           someobs: Loc.opt -> Loc.obs;
           coord: Loc.obs -> int -> float;
           cdiff: Loc.obs -> int -> float }

let make pdef =
  let crdtab = List.map (fun c -> Array.of_list c.P.c_range) pdef.P.p_coords
  and cur = ref (List.map (fun _ -> 0) pdef.P.p_coords) in
                                        (* the sensor starts at the origin *)
  let atobs () = !cur
  and toobs loc = cur := loc
  and someopt () = 
    List.map (fun c -> 
                if List.mem c.P.c_name pdef.P.p_find.P.m_coords 
                then Some (Random.int (List.length c.P.c_range))
                else None)
      pdef.P.p_coords
  and someobs opt =
    List.map2 (fun crd c ->
                 match crd with
                     None -> Random.int (List.length c.P.c_range)
                   | Some i -> i)
      opt pdef.P.p_coords
  and coord loc crd = (List.nth crdtab crd).(List.nth loc crd) in
  let cdiff loc crd = (coord loc crd) -. (coord !cur crd)
  in { atobs = atobs;
       toobs = toobs;
       someopt = someopt;
       someobs = someobs;
       coord = coord;
       cdiff = cdiff }
