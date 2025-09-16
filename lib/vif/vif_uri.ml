(* Part of this code is based on the furl project and
   Copyright (c) 2015 Gabriel Radanne <drupyo@zoho.com>
   SPDX-License-Identifier: MIT
   Copyright (c) 2025 Romain Calascibetta <romain.calascibetta@gmail.com>
*)

type 'a atom = 'a Tyre.t

type ('fu, 'return) path =
  | Host : string -> ('r, 'r) path
  | Rel : ('r, 'r) path
  | Path_const : ('f, 'r) path * string -> ('f, 'r) path
  | Path_atom : ('f, 'a -> 'r) path * 'a atom -> ('f, 'r) path

type nil = unit
type any = (string * string list) list

type ('fu, 'open_, 'return) query =
  | Nil : ('r, unit, 'r) query
  | Any : ('r, any,'r) query
  | Query_atom : string * 'a atom * ('f, 'open_, 'r) query -> ('a -> 'f, 'open_, 'r) query

type slash = Slash | No_slash | Maybe_slash
type ('f, 'open_, 'r) t = Url : slash * ('f, 'x) path * ('x, 'open_, 'r) query -> ('f, 'open_, 'r) t

module Path = struct
  let host str = Host str
  let relative = Rel
  let add path str = Path_const (path, str)
  let add_atom path atom = Path_atom (path, atom)

  let rec _concat : type f r x. (f, x) path -> (x, r) path -> (f, r) path =
   fun p1 p2 ->
    match p2 with
    | Host _ -> p1
    | Rel -> p1
    | Path_const (p, str) -> Path_const (_concat p1 p, str)
    | Path_atom (p, a) -> Path_atom (_concat p1 p, a)
end

module Query = struct
  let nil : (_, nil, _) query = Nil
  let any = Any
  let add n x query = Query_atom (n, x, query)

  let rec make_any : type f o r. (f, o, r) query -> (f, any, r) query = function
    | Nil -> Any
    | Any -> Any
    | Query_atom (n, x, q) -> Query_atom (n, x, make_any q)

  (*
  let rec _concat : type f o o' r x. (f, o, x) query -> (x, o', r) query -> (f, o', r) query =
   fun q1 q2 ->
    match q1 with
    | Nil -> q2
    | Any -> make_any q2
    | Query_atom (n, x, q) -> Query_atom (n, x, _concat q q2)
     *)
end

module Url = struct
  let make ?(slash = No_slash) path query : _ t = Url (slash, path, query)
end

let nil = Query.nil
let any = Query.any
let ( ** ) (n, x) q = Query.add n x q
let host = Path.host
let rel = Path.relative
let ( / ) = Path.add
let ( /% ) = Path.add_atom
let ( /? ) path query = Url.make ~slash:No_slash path query
let ( //? ) path query = Url.make ~slash:Slash path query
let ( /?? ) path query = Url.make ~slash:Maybe_slash path query
let eval_atom p x = Tyre.(eval (Internal.to_t p) x)

let eval_top_atom : type a. a Tyre.Internal.raw -> a -> string list = function
  | Opt p -> ( function None -> [] | Some x -> [ eval_atom p x ])
  | Rep p -> fun l -> List.of_seq (Seq.map (eval_atom p) l)
  | e -> fun x -> [ eval_atom e x ]

let rec eval_path : type r f.
    (f, r) path -> (string option -> string list -> r) -> f =
 fun p k ->
  match p with
  | Host str -> k (Some str) []
  | Rel -> k None []
  | Path_const (p, str) -> eval_path p (fun h r -> k h (str :: r))
  | Path_atom (p, a) ->
      let fn h r x = k h (eval_top_atom (Tyre.Internal.from_t a) x @ r) in
      eval_path p fn

let rec eval_query : type r o f.
    (f, o, r) query -> o -> ((string * string list) list -> r) -> f =
 fun q additional_queries k ->
  match q, additional_queries with
  | Nil, () -> k []
  | Any, additional_queries -> k additional_queries
  | Query_atom (n, a, q), additional_queries ->
      fun x ->
        let fn r = k ((n, eval_top_atom (Tyre.Internal.from_t a) x) :: r) in
        eval_query q additional_queries fn

let keval_additional_queries : ?slash:bool -> ('a, 'o, 'b) t -> 'o -> (string -> 'b) -> 'a =
 fun ?slash:(force = false) (Url (slash, p, q)) additional_queries k ->
  eval_path p @@ fun host path ->
  eval_query q additional_queries @@ fun query ->
  let path =
    match slash with Slash -> "" :: path | No_slash | Maybe_slash -> path
  in
  let host = Option.value ~default:"" host in
  let path =
    match path with
    | [] when force -> [ ""; "" ]
    | [] -> []
    | path -> "" :: List.rev path
  in
  let path = String.concat "/" path in
  let path = Pct.encode_path path in
  let query = Pct.encode_query query in
  k (host ^ path ^ query)

let eval_additional_queries ?slash t additional_queries = keval_additional_queries ?slash t additional_queries Fun.id

let no_additional_queries : type o. (_, o, _) t -> o =
  fun (Url (_, _, q)) ->
  let rec queries : type f o r. (f, o, r) query -> o = function
    | Any -> []
    | Nil -> ()
    | Query_atom (_, _, q) -> queries q
  in
  queries q

let keval ?slash t k =
  keval_additional_queries ?slash t (no_additional_queries t) k
and eval ?slash t =
  eval_additional_queries ?slash t (no_additional_queries t)

type 'a handler = 'a Httpcats.handler
type response = Httpcats.response
type error = Httpcats.error

let request ~fn a t = keval t @@ fun uri -> Httpcats.request ~fn ~uri a
