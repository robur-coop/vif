#require "vif" ;;

type foo =
  { username: string
  ; password: string
  ; age: int option
  ; address: string option }
;;

let foo =
  let open Json_encoding in
  let username = req "username" string in
  let password = req "password" string in
  let age = opt "age" int in
  let address = opt "address" string in
  let foo = obj4 username password age address in
  let prj { username; password; age; address } =
    (username, password, age, address)
  in
  let inj (username, password, age, address) =
    { username; password; age; address }
  in
  conv prj inj foo
;;

open Vif ;;

let deserialize req _server () =
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
      let* () = Response.with_string req str in
      Response.respond `OK
  | Error (`Msg msg) ->
      let* () = Response.with_string req msg in
      Response.respond (`Code 422)
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ post (json_encoding foo) (rel /?? nil) --> deserialize ]
;;

let default req target _server () =
  let str = Fmt.str "%s not found\n" target in
  let* () = Response.with_string req str in
  Response.respond `Not_found
;;

let () = Miou_unix.run @@ fun () -> Vif.run ~default routes () ;;
