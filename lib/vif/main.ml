#require "miou.unix" ;;
#require "mirage-crypto-rng-miou-unix" ;;
#require "vif" ;;
#require "digestif.c" ;;
#require "base64" ;;

type cfg = Config

let index _req server Config =
  Vif.Response.with_string server `OK "Hello from an OCaml script!"
;;

let hex _req arg server Config =
  Vif.Response.with_string server `OK (Fmt.str "%02x\n%!" arg)
;;

let digest req server Config =
  let ic = Vif.Request.to_stream req in
  let rec go ctx =
    match Vif.Stream.get ic with
    | Some str -> go (Digestif.SHA1.feed_string ctx str)
    | None -> Digestif.SHA1.get ctx
  in
  let hash = go Digestif.SHA1.empty in
  let hash = Digestif.SHA1.to_hex hash in
  Vif.Response.with_string server `OK hash
;;

let random req len server Config =
  let buf = Bytes.create 0x7ff in
  Vif.Response.with_stream server `OK @@ fun oc ->
  let rec go rem =
    if rem > 0 then begin
      let len = Int.min rem (Bytes.length buf) in
      Mirage_crypto_rng.generate_into buf len;
      let str = Bytes.sub_string buf 0 len in
      let str = Base64.encode_exn str in
      Vif.Stream.put oc str;
      go (rem - len)
    end
  in
  go len
;;

type foo =
  { username : string
  ; password : string
  ; age : int option }
;;

let foo =
  let open Json_encoding in
  let username = req "username" string in
  let password = req "password" string in
  let age = opt "age" int in
  let foo = obj3 username password age in
  let inj (username, password, age) = { username; password; age } in
  let prj { username; password; age } = (username, password, age) in
  conv prj inj foo
;;

let login req server Config =
  match Vif.Request.to_json req with
  | Ok (foo : foo) ->
      Logs.debug (fun m -> m "username: %s" foo.username);
      Logs.debug (fun m -> m "password: %s" foo.password);
      Vif.Response.with_string server `OK "Foo"
  | Error (`Msg err) ->
      Logs.err (fun m -> m "Invalid JSON: %s" err);
      Vif.Response.with_string server `Not_acceptable err

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [
    get (rel /?? nil) --> index
  ; get (rel / "random" /% Tyre.int /?? nil) --> random
  ; get (rel / "hex" /% Tyre.int /?? nil) --> hex
  ; post any (rel / "digest" /?? nil) --> digest
  ; post (json_encoding foo) (rel / "json" /?? nil) --> login
  ]
;;

let default req target server Config =
  Logs.debug (fun m -> m "We are into the default case");
  Vif.Response.with_string server `Not_found (Fmt.str "%S not found\n%!" target)
;;

let rng =
  let open Mirage_crypto_rng_miou_unix in
  let finally = kill in
  Vif.D.device ~name:"rng" ~finally [] @@ fun Config ->
  initialize (module Pfortuna)
;;

let () =
  Miou_unix.run @@ fun () ->
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  let cfg = Vif.config sockaddr in
  Vif.run ~cfg ~default ~devices:Vif.Ds.[ rng ] routes Config
;;
