let () = Logs_threaded.enable ()
let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let run () roots stdlib main =
  let roots = List.map Fpath.to_string roots in
  let cfg = Vif_top.config ~stdlib roots in
  let lines =
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
  match Vif_top.eval ~filename:(Fpath.to_string main) cfg ~lines with
  | Ok () -> ()
  | Error err ->
      Fmt.epr "%a%!" (Vif_top.pp_error ?file:None) err;
      exit 1

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

let term =
  let open Term in
  const run $ Vif.setup_config $ Vif_mmeta.setup $ setup_stdlib $ main

let cmd =
  let doc = "vif" in
  let man = [] in
  let info = Cmd.info "vif" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
