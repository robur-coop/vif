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
