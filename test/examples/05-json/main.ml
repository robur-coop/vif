#require "vif" ;;

type foo =
  { username: string
  ; password: string
  ; age: int option
  ; address: string option }
;;

let foo =
  let open Jsont in
  let username = Object.mem "username" string in
  let password = Object.mem "password" string in
  let age = Object.opt_mem "age" int in
  let address = Object.opt_mem "address" string in
  let fn username password age address =
    { username; password; age; address } in
  Object.map fn
  |> username
  |> password
  |> age
  |> address
  |> Object.finish
;;

open Vif ;;

let deserialize req _server () =
  let open Vif.Response.Syntax in
  match Vif.Request.of_json req with
  | Ok (foo : foo) ->
      let str =
        Fmt.str "username: %s, password: %s, age: %a, address: %a\n"
          foo.username foo.password
          Fmt.(Dump.option int)
          foo.age
          Fmt.(Dump.option string)
          foo.address
      in
      let* () = Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () = Response.with_string req str in
      Response.respond `OK
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "Invalid JSON: %s" msg);
      let* () = Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () = Response.with_string req msg in
      Response.respond (`Code 422)
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (json_encoding foo) (rel /?? nil) --> deserialize ]
;;

let () = Miou_unix.run @@ fun () -> Vif.run routes () ;;
