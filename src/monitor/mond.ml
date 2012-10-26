(* $Id: mond.ml 1112 2012-10-26 20:19:40Z tolpin $ *)

module Jt = Json_type 

(* MONITOR SERVER *)

(* most of the job is done in Javascript on the client side;
   the server reads logs incrementally on requests,
   the server activity is logged on console by the name of the log and the number of lines read:
   svm.obs: 10
   svn.obs: 2
   svn.opn: 100 *)

(* Parsing Logs *)

type logline = 
    Names of string list
  | Types of int list
  | Values of float list
  | Stage of string

(* read lines from the log starting with the given position *)
let readlines ?(maxlines=(-1)) fname pos =
  (* if pos==0, read two lines, first is names, second is types; otherwise
     read as many lines as available, values or stage. 
     use input_char, remember position of last end-of-line, return it;
     if the file does not exist, return 0 lines, maybe it's too early *)

  let input_line inp = (* read a line terminated by newline *)
    let buffer = Buffer.create 16 in
    let rec loop () =
      try
        let ch = input_char inp in
          match ch with
              '\r' -> loop () 
            | '\n' -> Some (Buffer.contents buffer)
            | ch -> ( Buffer.add_char buffer ch
                    ; loop () )
      with End_of_file -> None 
    in loop ()

  and split = Str.split (Str.regexp "[ \t]+") in
  let names_of_line line = Names (split line)
  and types_of_line line = Types (List.map int_of_string (split line))
  and values_of_line line = Values (List.map float_of_string (split line))
  and stage_of_line line = Stage (String.sub line 2 (String.length line - 2)) in

  let input_header inp = (* read two lines, interpret as names and types *)
    let namesline = input_line inp in
    let typesline = input_line inp 
    in match namesline, typesline with 
        Some namesline, Some typesline ->
          [names_of_line namesline; types_of_line typesline], pos_in inp
      | _ -> ([], 0)
  and input_data inp = (* read multiple lines, interpret as values or stage *)
    let rec loop lines pos =
      match (if List.length lines = maxlines
             then None
             else input_line inp) with
          Some line -> 
            loop
              ((if line.[0]='#'
                then stage_of_line
                else values_of_line) line::lines)
              (pos_in inp)
        | None -> List.rev lines, pos
    in loop [] pos
  in
    try 
      let inp = open_in fname
      in if in_channel_length inp < pos (* truncated, reset *)
        then ( close_in inp
             ; raise (Sys_error (Printf.sprintf "%s: file too short" fname) ) )
        else ( seek_in inp pos
             ; let lines =
                 match pos with
                     0 -> input_header inp
                   | _ -> input_data inp
               in ( close_in inp
                  ; lines ) )
    with Sys_error _ -> (* wrong name, reset or not yet created *) [], 0

(* Json_type wrapper *)
let json_of_line = function
    Names names -> Jt.Object
      [ "linetype", Jt.String "names";
        "names", Jt.Array (List.map (fun n -> Jt.String n) names) ]
  | Types types -> Jt.Object 
      [ "linetype", Jt.String "types";
        "types", Jt.Array (List.map (fun t -> Jt.Int t) types) ]
  | Values values -> Jt.Object
      [ "linetype", Jt.String "values";
        "values", Jt.Array (List.map (fun v -> Jt.Float v) values) ]
  | Stage stage -> Jt.Object 
      [ "linetype", Jt.String "stage";
        "stage", Jt.String stage ]

let json_of_lines (lines, pos) =
  Jt.Object [ "lines", Jt.Array (List.map json_of_line lines);
              "pos", Jt.Int pos ]

;;

(* Processing Queries *)
let content_type = ref "application/json"
and quiet = ref false
and maxlines = ref (-1)
and cwd = Sys.getcwd () in (* process the query in the current directory *)
let process_query (cgi: Netcgi.cgi_activation) = Sys.chdir cwd ;
  try
    ( cgi # set_header ~cache: `No_cache ~content_type: !content_type ()
    ; cgi # output # output_string
       (Json_io.string_of_json
          (let fname = cgi # argument_value "fname"
           and pos = int_of_string (cgi # argument_value "pos") in
           let lines = readlines ~maxlines:!maxlines fname pos
           in ( if not !quiet then Printf.printf "%s: %d\n%!" fname (List.length (fst lines))
              ; json_of_lines lines )))
    ; cgi # output # commit_work () )
  with error ->
	( cgi # output # rollback_work()
        ; cgi # set_header ~status: `Forbidden
            ~cache: `No_cache  ~content_type: "text/plain" ()
        ; cgi # output # output_string (Printexc.to_string error) 
        ; cgi # output # commit_work () 
        ; if not !quiet then
            Printf.eprintf "error while reading %s: %s\n%!"
              (cgi # argument_value "fname") (Printexc.to_string error) ) in
let start () =
  let opt_list, cmdline_cfg = Netplex_main.args() in
  let () = Arg.parse 
    (opt_list @
       ["-ct", Arg.Set_string content_type,
        "<content-type>  Specify content-type for json responses, default is application/json";
        "-q", Arg.Set quiet, 
        " Suppress printing of progress messages";
        "-l", Arg.Set_int maxlines,
        " Limit the number of data lines per query, default is no limit"])
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: mond [options]" in
  let tail =
    { Nethttpd_services.dyn_handler =
        (fun _ ->
           fun cgi ->
             process_query cgi);
      dyn_activation = Nethttpd_services.std_activation `Std_activation_unbuffered;
      dyn_uri = None;                 (* not needed *)
      dyn_translator = (fun _ -> ""); (* not needed *)
      dyn_accept_all_conditionals = false; } in
  let nethttpd_factory = 
    Nethttpd_plex.nethttpd_factory ~handlers:[ "tail", tail ] ()
  in Netplex_main.startup
       ( Netplex_mp.mp ())
       Netplex_log.logger_factories
       Netplex_workload.workload_manager_factories
       [ nethttpd_factory ]
       cmdline_cfg
in ( Sys.set_signal Sys.sigpipe Sys.Signal_ignore
   ; start () )

(* Testing *)

let dumplog fname = (* convert the log file contents into a jason string *)
  let headerlines, pos = readlines fname 0 in
  let bodylines, pos = readlines fname pos 
  in Json_io.string_of_json (json_of_lines (headerlines @ bodylines, pos))




