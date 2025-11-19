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

type 'a t = { rwit: 'a Witness.t; rfields: 'a fields_and_constr }

and 'a fields_and_constr =
  | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

and ('a, 'b) fields =
  | F0 : ('a, 'a) fields
  | F1 : 'b field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

and 'a field = { fname: string; ftype: 'a atom }
and 'a atom = Primary : 'a primary -> 'a atom | Record : 'a t -> 'a atom
and 'a primary = String : string primary | Int : int primary

type meta = {
    name: string option
  ; filename: string option
  ; size: int option
  ; mime: string option
}

let pp_meta ppf t =
  match (t.name, t.filename) with
  | Some name, _ -> Fmt.string ppf name
  | _, Some filename -> Fmt.string ppf filename
  | _ -> Fmt.pf ppf "<unknown-part>"

type raw = ((meta * Vif_headers.t) * string) list
type stream = (meta * string Flux.source) Flux.stream

module Fields_folder (Acc : sig
  type ('a, 'b) t
end) =
struct
  type 'a t = {
      nil: ('a, 'a) Acc.t
    ; cons: 'b 'c. 'b field -> ('a, 'c) Acc.t -> ('a, 'b -> 'c) Acc.t
  }

  let rec fold : type a c. a t -> (a, c) fields -> (a, c) Acc.t =
   fun folder -> function
    | F0 -> folder.nil
    | F1 (f, fs) -> folder.cons f (fold folder fs)
end

module Record_get = Fields_folder (struct
  type ('a, 'b) t = raw -> 'b -> 'a
end)

exception Field_not_found of string

let find_by_name name raw =
  let fn ((meta, _), _) =
    match meta.name with Some name' -> String.equal name name' | None -> false
  in
  let _, value =
    try List.find fn raw with Not_found -> raise (Field_not_found name)
  in
  value

let rec get_value : type a. a atom -> string -> raw -> a = function
  | Primary String -> find_by_name
  | Primary Int -> fun name raw -> int_of_string (find_by_name name raw)
  | Record r -> fun _ raw -> get_record r raw

and get_record : type a. a t -> raw -> a =
 fun { rfields= Fields (fs, constr); _ } ->
  let nil _raw fn = fn in
  let cons { fname; ftype } k =
    let get = get_value ftype fname in
    fun raw constr ->
      let x = get raw in
      let constr = constr x in
      k raw constr
  in
  let fn = Record_get.fold { nil; cons } fs in
  fun raw -> fn raw constr

type ('a, 'b, 'c) orecord = ('a, 'c) fields -> 'b * ('a, 'b) fields
type 'a a_field = Field : 'x field -> 'a a_field

let field fname ftype = { fname; ftype }
let record : 'b -> ('a, 'b, 'b) orecord = fun c fs -> (c, fs)

module SSet = Set.Make (String)

let check_unique fn =
  let rec go s = function
    | [] -> ()
    | x :: xs -> (
        match SSet.find_opt x s with
        | None -> go (SSet.add x s) xs
        | Some _ -> fn x)
  in
  go SSet.empty

let check_uniq_field_names rfields =
  let names = List.map (fun (Field { fname; _ }) -> fname) rfields in
  let failure fname =
    Fmt.invalid_arg "The name %s was used for two or more parts." fname
  in
  check_unique failure names

let fields r =
  let rec go : type a b. (a, b) fields -> a a_field list = function
    | F0 -> []
    | F1 (h, t) -> Field h :: go t
  in
  match r.rfields with Fields (f, _) -> go f

let app : type a b c d. (a, b, c -> d) orecord -> c field -> (a, b, d) orecord =
 fun r f fs -> r (F1 (f, fs))

let sealr : type a b. (a, b, a) orecord -> a t =
 fun r ->
  let c, fs = r F0 in
  let rwit = Witness.make () in
  let sealed = { rwit; rfields= Fields (fs, c) } in
  check_uniq_field_names (fields sealed);
  sealed

let ( |+ ) = app
let string = Primary String
let int = Primary Int
