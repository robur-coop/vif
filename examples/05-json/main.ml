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

let deserialize req server () =
  match Vif.Request.to_json req with
  | Ok (foo : foo) ->
      let str =
        Fmt.str "username: %s, password: %s, age: %a, address: %a\n"
          foo.username foo.password
          Fmt.(Dump.option int)
          foo.age
          Fmt.(Dump.option string)
          foo.address
      in
      Vif.Response.with_string server `OK str
  | Error (`Msg msg) -> Vif.Response.with_string server (`Code 422) msg
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ post (json_encoding foo) (rel /?? nil) --> deserialize ]
;;

let default req target server () =
  let str = Fmt.str "%s not found\n" target in
  Vif.Response.with_string server `Not_found str
;;

let () = Miou_unix.run @@ fun () -> Vif.run ~default routes () ;;
