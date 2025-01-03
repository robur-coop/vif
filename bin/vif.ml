let reporter ppf =
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

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let main =
  [
    {ocaml|let v = 42;;|ocaml}
  ; {ocaml|let rec infinity () = infinity ();;|ocaml}
  ; {ocaml|print_endline "Hello World!";;|ocaml}
  ; {ocaml|let a = Bool.to_int true;;|ocaml}; {ocaml|#show_dirs;;|ocaml}
  ; {ocaml|#directory "/home/dinosaure/.opam/5.2.0+ocamlnat/lib/ocaml/";;|ocaml}
  ; {ocaml|#load "/home/dinosaure/.opam/5.2.0+ocamlnat/lib/ocaml/stdlib.cmxa";;|ocaml}
  ; {ocaml|#require "miou";;|ocaml}
  ; {ocaml|let fn () = print_endline "Hello from Miou!";;|ocaml}
  ; {ocaml|Miou.run fn;;|ocaml}
  ]

let stdlib = Fpath.v "/home/dinosaure/.opam/5.2.0+ocamlnat/lib/ocaml/"

let run roots =
  let cfg = Vif_top.config ~stdlib roots in
  match Vif_top.eval cfg main with
  | Ok sstr -> List.iter print_endline sstr
  | Error sstr -> List.iter prerr_endline sstr

open Cmdliner

let term =
  let open Term in
  const run $ Vif_meta.setup

let cmd =
  let doc = "vif" in
  let man = [] in
  let info = Cmd.info "vif" ~doc ~man in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
