type null = Null
type json = Json
type multipart_form = Multipart_form
type _0 = L0
type _1 = L1

type ('c, 'a, 'l) t' =
  | Null : (null, unit, _0) t'
  | Json_encoding : 'a Jsont.t -> (json, 'a, _0) t'
  | Multipart_form_encoding :
      'a Vif_multipart_form.t
      -> (multipart_form, 'a, _0) t'
  | Multipart_form : (multipart_form, Vif_multipart_form.stream, _0) t'
  | Option : ('c, 'a, _0) t' -> ('c, 'a option, _1) t'
  | Any : ('c, string, _0) t'

type ('c, 'a) t = Type : ('c, 'a, _) t' -> ('c, 'a) t

let null = Type Null
let json_encoding e = Type (Json_encoding e)
let m e = Type (Multipart_form_encoding e)
let multipart_form = Type Multipart_form
let any = Type Any

let option : type a c. (c, a) t -> (c, a option) t =
 fun (Type value) ->
  match value with
  | Option _ -> invalid_arg "Vif_type.option"
  | Null -> Type (Option Null)
  | Json_encoding e -> Type (Option (Json_encoding e))
  | Multipart_form_encoding e -> Type (Option (Multipart_form_encoding e))
  | Multipart_form -> Type (Option Multipart_form)
  | Any -> Type (Option Any)
