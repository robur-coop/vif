#require "tyxml-ppx" ;;
#require "tyxml" ;;
#require "vif" ;;
#require "uuidm" ;;
#require "mirage-crypto-rng" ;;

open Tyxml ;;
open Vif ;;

let%html form = {html|
<html>
  <head>
    <title>Vif!</title>
    <script type="text/javascript" src="main.js"></script>
  </head>
  <body>
    <form method="post" enctype="multipart/form-data" action="upload">
      <input type="file" name="image" id="image" onchange="uploadFile()" />
      <progress id="progressBar" value="0" max="100" style="width: 300px;"></progress>
      <h3 id="status"></h3>
      <p id="loaded_n_total"></p>
    </form>
  </body>
</html>
|html} ;;

let form : Tyxml_html.doc = form ;;

let default req _server cfg =
  let* () = Response.with_tyxml req form in
  Response.respond `OK
;;

let upload req server cfg =
  let rec rand () =
    let uuidm = Uuidm.v8 (Mirage_crypto_rng.generate 16) in
    if Sys.file_exists (Uuidm.to_string uuidm)
    then rand () else uuidm  in
  let fn (part, src) =
    Logs.debug (fun m -> m "Got a new part");
    match Multipart_form.name part with
    | Some "image" ->
        let filename = rand () in
        let filename = Uuidm.to_string filename in
        S.Stream.to_file filename (S.Stream.from src)
    | _ ->
        S.Stream.(drain (from src)) in
  let stream = Result.get_ok (Request.of_multipart_form req) in
  S.Stream.each fn stream;
  let* () = Response.with_string req "ok\n" in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel /?? nil) --> default
  ; post multipart_form (rel / "upload" /?? nil) --> upload ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~handlers:[ Handler.static ] routes ()
;;
