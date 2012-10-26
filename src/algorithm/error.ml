(* $Id: error.ml 985 2010-06-28 17:28:30Z tolpin $ *)

module P = Pdefinition
module O = Object

(* problem definition error *)
let definition_error e maybe_pos =
  ( Printf.eprintf "definition error"
  ; ( match maybe_pos with
          Some { Lexing.pos_fname = fname;
                 Lexing.pos_lnum = lnum } -> Printf.eprintf " at %s:%d" fname lnum
        | None -> () )
  ; Printf.eprintf ": "
  ; ( match e with
          P.Illegal_char c -> Printf.eprintf "illegal character '%c'" c
        | P.Syntax_error -> Printf.eprintf "syntax error"
        | P.Undecl_coord cname -> Printf.eprintf "undeclared coordinate '%s'" cname
        | P.Undefn_coord cname -> Printf.eprintf "undefined coordinate '%s'" cname
        | P.Undecl_featr fname -> Printf.eprintf "undeclared feature '%s'" fname
        | P.Unobsv_featr fname -> Printf.eprintf "undefined feature '%s'" fname
        | P.Arities_differ -> Printf.eprintf "tuples must be of the same size"
        | P.Unbound_name name -> Printf.eprintf "unbound name '%s' in expression" name
        | P.Invalid_sect name -> Printf.eprintf "invalid section for coordinate '%s'" name
        | P.Wrong_funtyp fname -> Printf.eprintf "wrong function type for '%s'" fname
        | P.Unavail_func fname -> Printf.eprintf "function '%s' unavailable" fname
        | P.Wrong_contxt -> Printf.eprintf "wrong subexpression context" )
  ; Printf.eprintf "\n" )

(* object data error *)
let data_error e =
  ( Printf.eprintf "data error: "
  ; ( match e with
          O.Unknown_varnam name -> Printf.eprintf "unknown variable name '%s'" name
        | O.Invalid_coltyp typ -> Printf.eprintf "invalid column type %s" typ
        | O.Wrong_colcount -> Printf.eprintf "invalid column count"
        | O.Corrupt_data -> Printf.eprintf "corrupt data" )
  ; Printf.eprintf "\n" )

(* general error *)
let error e = Printf.eprintf "error: %s\n" e
