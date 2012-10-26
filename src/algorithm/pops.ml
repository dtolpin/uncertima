(* $Id: pops.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition

let parse pname =
  let ich = open_in pname
  and lex_zero_p = (* ocaml library does not initialize file name *)
    { Lexing.pos_fname = pname;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0; } in
  let lexbuf = Lexing.from_channel ich
  in ( lexbuf.Lexing.lex_start_p <- lex_zero_p
     ; lexbuf.Lexing.lex_curr_p <- lex_zero_p
     ; let pdef = Pparser.problem Plexer.token lexbuf
       in (close_in ich; pdef) )

let enumerate l = 
  let rec number l i li = 
    match l with
        [] -> List.rev li
      | h::t -> number t (i+1) ((h,i)::li)
  in number l 0 []

let tabulate coords = 
      List.map (fun c -> (c.P.c_name, enumerate c.P.c_range)) coords
