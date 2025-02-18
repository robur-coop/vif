type null = Null
type json = Json
type multipart_form = Multipart_form
type stream = string Stream.source Multipart_form.elt Stream.source

type ('c, 'a) t =
  | Null : (null, unit) t
  | Json_encoding : 'a Json_encoding.encoding -> (json, 'a) t
  | Multipart_form_encoding : 'a Multipart_form.t -> (multipart_form, 'a) t
  | Multipart_form : (multipart_form, stream) t
  | Json : (json, Json.t) t
  | Any : ('c, string) t

let null = Null
let json_encoding e = Json_encoding e
let json = Json
let any = Any

module Witness = struct
  type (_, _) eq = Refl : ('a, 'a) eq
  type _ equality = ..

  module type Inst = sig
    type t
    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make : type a. unit -> a t =
   fun () ->
    let module Inst = struct
      type t = a
      type _ equality += Eq : t equality
    end in
    (module Inst)

  let _eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
end

module Multipart = struct
  type 'a t = { rwit: 'a Witness.t; rfields: 'a fields_and_constr }

  and 'a fields_and_constr =
    | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

  and ('a, 'b) fields =
    | F0 : ('a, 'a) fields
    | F1 : ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

  and ('a, 'b) field = { name: string; ftype: 'b ty; fget: 'a -> 'b }
  and 'a ty = Primary : 'a primary -> 'a ty | Record : 'a t -> 'a ty
  and 'a primary = String : string primary
end
