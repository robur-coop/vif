type null = Null
type json = Json
type multipart_form = Multipart_form
type stream = string Stream.source Multipart_form.elt Stream.source

type ('c, 'a) t =
  | Null : (null, unit) t
  | Json_encoding : 'a Json_encoding.encoding -> (json, 'a) t
  | Multipart_form_encoding : 'a Vif_multipart_form.t -> (multipart_form, 'a) t
  | Multipart_form : (multipart_form, stream) t
  | Json : (json, Json.t) t
  | Any : ('c, string) t

let null = Null
let json_encoding e = Json_encoding e
let m e = Multipart_form_encoding e
let json = Json
let any = Any
