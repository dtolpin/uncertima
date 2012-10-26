(* $Id: debug.ml 985 2010-06-28 17:28:30Z tolpin $ *)

let info l s =
  if !Conf.debug then Printf.eprintf "DEBUG [%s]: %s\n%!" l s
