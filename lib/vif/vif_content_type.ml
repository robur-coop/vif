type null = Null
and json = Json

type ('c, 'a) t =
  | Null : (null, unit) t
  | Json_encoding : 'a Json_encoding.encoding -> (json, 'a) t
  | Json : (json, Json.t) t
  | Any : ('c, string) t

let null = Null
let json_encoding e = Json_encoding e
let json = Json
let any = Any
