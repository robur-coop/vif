#require "tyxml-ppx" ;;
#require "tyxml" ;;
#require "vif" ;;
#require "uuidm" ;;
#require "mirage-crypto-rng" ;;

open Tyxml ;;

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
  let open Vif.Response.Syntax in
  let* () = Vif.Response.with_tyxml req form in
  Vif.Response.respond `OK
;;

let upload req server cfg =
  let rec rand () =
    let uuidm = Uuidm.v8 (Mirage_crypto_rng.generate 16) in
    if Sys.file_exists (Uuidm.to_string uuidm)
    then rand () else uuidm  in
  let fn (part, src) =
    match Vif.Multipart_form.name part with
    | Some "image" ->
        let filename = rand () in
        let filename = Uuidm.to_string filename in
        Vif.Stream.Stream.to_file filename (Vif.Stream.Stream.from src)
    | _ ->
        Vif.Stream.Stream.(drain (from src)) in
  let open Vif.Response.Syntax in
  let stream = Result.get_ok (Vif.Request.of_multipart_form req) in
  Vif.Stream.Stream.each fn stream;
  let* () = Vif.Response.with_string req "ok\n" in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ get (rel /?? nil) --> default
  ; post multipart_form (rel / "upload" /?? nil) --> upload ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~handlers:[ Vif.Handler.static ] routes ()
;;
