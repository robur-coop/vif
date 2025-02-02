let () = Logs_threaded.enable ()

let _reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let run _quiet () roots stdlib main =
  let roots = List.map Fpath.to_string roots in
  let cfg = Vif_top.config ~stdlib roots in
  let main =
    let ic = open_in (Fpath.to_string main) in
    let finally () = close_in ic in
    Fun.protect ~finally @@ fun () ->
    let rec go acc =
      match input_line ic with
      | line -> go (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    go []
  in
  match Vif_top.eval cfg main with Ok () -> () | Error () -> exit 1

open Cmdliner

let main =
  let doc = "The OCaml script to execute." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as value when Sys.file_exists str && Sys.is_directory str = false ->
        value
    | Ok value -> error_msgf "%a does not exists" Fpath.pp value
    | Error _ as err -> err
  in
  let existing_file = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  required & pos 0 (some existing_file) None & info [] ~doc ~docv:"FILE"

let setup_stdlib () =
  let cmd = Bos.Cmd.(v "ocamlopt" % "-config") in
  let ( let* ) = Result.bind in
  let* exists = Bos.OS.Cmd.exists cmd in
  if exists then
    let r = Bos.OS.Cmd.run_out cmd in
    let* kvs, _ = Bos.OS.Cmd.out_lines ~trim:true r in
    let kvs = List.map Astring.String.fields kvs in
    let kvs =
      List.fold_left
        (fun acc -> function k :: v :: _ -> (k, v) :: acc | _ -> acc)
        [] kvs
    in
    match List.assoc_opt "standard_library:" kvs with
    | Some stdlib -> Fpath.of_string stdlib
    | None ->
        error_msgf "Impossible to know where is the OCaml standard library"
  else error_msgf "ocamlopt is not available"

let setup_stdlib () =
  match setup_stdlib () with
  | Ok stdlib -> `Ok stdlib
  | Error (`Msg msg) -> `Error (false, Fmt.str "%s." msg)

let setup_stdlib =
  let open Term in
  ret (const setup_stdlib $ const ())

let docs_output = "OUTPUT"

let verbosity =
  let env = Cmd.Env.info "VIF_LOGS" in
  Logs_cli.level ~env ~docs:docs_output ()

let renderer =
  let env = Cmd.Env.info "VIF_FMT" in
  Fmt_cli.style_renderer ~env ~docs:docs_output ()

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "VIF_UTF_8" in
  let open Arg in
  value
  & opt bool true
  & info [ "with-utf-8" ] ~doc ~docv:"BOOL" ~docs:docs_output ~env

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.Error ->
      let h = Option.value ~default:"ERROR" h in
      pp_h ppf err_style h
  | Logs.Warning ->
      let h = Option.value ~default:"WARN" h in
      pp_h ppf warn_style h
  | Logs.Info ->
      let h = Option.value ~default:"INFO" h in
      pp_h ppf info_style h
  | Logs.Debug ->
      let h = Option.value ~default:"DEBUG" h in
      pp_h ppf debug_style h
  | Logs.App ->
      Fun.flip Option.iter h @@ fun h ->
      Fmt.pf ppf "[%a] " Fmt.(styled app_style (fmt "%10s")) h

let pp_header =
  let pp_h ppf style h = Fmt.pf ppf "[%a]" Fmt.(styled style (fmt "%10s")) h in
  pp_header ~pp_h

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%02d]%a[%a]: @[<hov>" ^^ fmt ^^ "@]\n%!")
        (Stdlib.Domain.self () :> int)
        pp_header (level, header)
        Fmt.(styled `Magenta (fmt "%20s"))
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  let stdout =
    Format.make_formatter (output_substring stdout) (fun () -> flush stdout)
  in
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  let reporter = reporter Fmt.stderr in
  Logs.set_reporter reporter;
  Logs.set_level level;
  (Option.is_none level, stdout)

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let term =
  let open Term in
  const run
  $ setup_logs
  $ Vif.setup_config
  $ Vif_meta.setup
  $ setup_stdlib
  $ main

let cmd =
  let doc = "vif" in
  let man = [] in
  let info = Cmd.info "vif" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
