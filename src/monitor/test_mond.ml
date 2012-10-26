(* $Id: test_mond.ml 840 2010-02-17 18:06:14Z tolpin $ *)

(* testing calls for mond *)

if Array.length Sys.argv=2 then print_endline ( Mond.dumplog Sys.argv.(1) )
else prerr_endline "usage: dumplog <log-file-name>"

