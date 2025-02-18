type t =
  [ `CONNECT
  | `DELETE
  | `GET
  | `HEAD
  | `OPTIONS
  | `POST
  | `PUT
  | `TRACE
  | `Other of string ]

let pp ppf = function
  | `CONNECT -> Fmt.string ppf "CONNECT"
  | `DELETE -> Fmt.string ppf "DELETE"
  | `GET -> Fmt.string ppf "GET"
  | `HEAD -> Fmt.string ppf "HEAD"
  | `OPTIONS -> Fmt.string ppf "OPTIONS"
  | `POST -> Fmt.string ppf "POST"
  | `PUT -> Fmt.string ppf "PUT"
  | `TRACE -> Fmt.string ppf "TRACE"
  | `Other str -> Fmt.string ppf (String.uppercase_ascii str)
