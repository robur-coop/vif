let src = Logs.Src.create "vif.cookie"

module Log = (val Logs.src_log src : Logs.LOG)

let prefix req0 =
  let target = Vif_request0.target req0 in
  let secure =
    Option.is_some (Vif_request0.tls req0) || Vif_request0.on_localhost req0
  in
  match (target, secure) with
  | "/", true -> "__Host-"
  | _, true -> "__Secure-"
  | _ -> ""

let is_cookie key = String.lowercase_ascii key = "cookie"

let all_cookies hdrs =
  let cookies = List.filter (fun (k, _) -> is_cookie k) hdrs in
  let cookies = List.map snd cookies in
  let cookies = List.map (String.split_on_char ';') cookies in
  let cookies = List.flatten cookies in
  let fn acc str =
    match String.split_on_char '=' str with
    | [] -> assert false
    | [ k; v ] ->
        let k = String.trim k and v = String.trim v in
        (k, v) :: acc
    | _ -> acc
  in
  List.fold_left fn [] cookies

let guard error fn = if fn () then Ok () else Error error
let err_cookie = `Invalid_encrypted_cookie

let pp_error ppf = function
  | `Invalid_encrypted_cookie -> Fmt.string ppf "Invalid encrypted cookie"
  | `Not_found -> Fmt.string ppf "Cookie not found"
  | `Msg str -> Fmt.string ppf str

let get ?(encrypted = true) ~name server req0 =
  let hdrs = Vif_request0.headers req0 in
  let prefix = prefix req0 in
  let name = prefix ^ name in
  match List.assoc_opt name (all_cookies hdrs) with
  | None -> Error `Not_found
  | Some value when encrypted ->
      let ( let* ) = Result.bind in
      let alphabet = Base64.uri_safe_alphabet in
      let* value = Base64.decode ~pad:false ~alphabet value in
      let err = `Invalid_encrypted_cookie in
      let* () = guard err @@ fun () -> String.length value >= 14 in
      let* () = guard err @@ fun () -> value.[0] == '\x00' in
      let nonce = String.sub value 1 12 in
      let adata = "vif.cookie-" ^ name in
      let vdata = String.sub value 13 (String.length value - 13) in
      let key = Vif_s.cookie_key server in
      let value =
        Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce ~adata vdata
      in
      let* () = guard err @@ fun () -> Option.is_some value in
      Ok (Option.get value)
  | Some value -> Ok value

type config = {
    expires: float option
  ; max_age: float option
  ; domain: [ `host ] Domain_name.t option
  ; path: bool
  ; secure: bool
  ; http_only: bool
  ; same_site: [ `Strict | `Lax | `None ]
}

let default_config =
  {
    expires= None
  ; max_age= None
  ; domain= None
  ; path= true
  ; secure= true
  ; http_only= true
  ; same_site= `Lax
  }

let config ?expires ?max_age ?domain ?(path = true) ?(secure = true)
    ?(http_only = true) ?(same_site = `Lax) () =
  { expires; max_age; domain; path; secure; http_only; same_site }

let set_cookie cfg ~path name value =
  let expires = None in
  let max_age = None in
  let domain = Option.map (Fmt.str "Domain=%a" Domain_name.pp) cfg.domain in
  (* TODO(dinosaure): warn if cfg.domain != req0.tls.peer_name? *)
  let path = if cfg.path then Some (Fmt.str "Path=%s" path) else None in
  let secure = if cfg.secure then Some "Secure" else None in
  let http_only = if cfg.http_only then Some "HttpOnly" else None in
  let same_site =
    match cfg.same_site with
    | `Lax -> Some "SameSite=Lax"
    | `Strict -> Some "SameSite=Strict"
    | `None -> Some "SameSite=None"
  in
  let attributes =
    List.filter_map Fun.id
      [ expires; max_age; domain; path; secure; http_only; same_site ]
  in
  Fmt.str "%s=%s; %a" name value Fmt.(list ~sep:(any "; ") string) attributes

let random len = Mirage_crypto_rng.generate len

let set ?(encrypt = true) ?(cfg = default_config) ?(path = "/") ~name server
    req0 value =
  let secure =
    Option.is_some (Vif_request0.tls req0) || Vif_request0.on_localhost req0
  in
  let prefix =
    match (cfg.secure, cfg.domain, cfg.path, secure, path) with
    | true, None, true, true, "/" -> "__Host-"
    | true, _, _, true, _ -> "__Secure-"
    | _ -> ""
  in
  let name = prefix ^ name in
  if encrypt then
    let key = Vif_s.cookie_key server in
    let nonce = random 12 in
    let adata = "vif.cookie-" ^ name in
    let value =
      Mirage_crypto.AES.GCM.authenticate_encrypt ~key ~nonce ~adata value
    in
    let alphabet = Base64.uri_safe_alphabet in
    let value = "\x00" ^ nonce ^ value in
    let value = Base64.encode_exn ~pad:false ~alphabet value in
    let value = set_cookie cfg ~path name value in
    Vif_response.add ~field:"set-cookie" value
  else
    let value = set_cookie cfg ~path name value in
    Vif_response.add ~field:"set-cookie" value

let set ?encrypt ?cfg ?path ~name server req value =
  set ?encrypt ?cfg ?path ~name server req.Vif_request.request value
