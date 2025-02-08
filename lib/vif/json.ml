type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
type t = [ value | `A of t list | `O of (string * t) list ]
type 'a or_error = ('a, [ `Msg of string ]) result
type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let _max_young_size = 0x7ff

let rec pp ppf = function
  | `Null -> Fmt.const Fmt.string "()" ppf ()
  | `Bool v -> Fmt.bool ppf v
  | `Float v -> Fmt.float ppf v
  | `Name v -> Fmt.string ppf v
  | `String v -> Fmt.string ppf v
  | `O lst -> Fmt.Dump.list (Fmt.Dump.pair Fmt.string pp) ppf lst
  | `A lst -> Fmt.Dump.list pp ppf lst

let decode ~input k =
  let decoder = Jsonm.decoder `Manual in
  let rec await k `Await =
    match input () with
    | Some str ->
        let buf = Bytes.unsafe_of_string str in
        Jsonm.Manual.src decoder buf 0 (Bytes.length buf);
        k ()
    | None ->
        Jsonm.Manual.src decoder Bytes.empty 0 0;
        k ()
  and error (`Error err) =
    error_msgf "Invalid JSON input: %a" Jsonm.pp_error err
  and end_of_input `End = error_msgf "Unexpected end of input"
  and arr acc k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> arr acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> core (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> name n k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> core (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> obj acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v ->
        error_msgf "Unexpected lexeme: %a (expected key)" Jsonm.pp_lexeme v
  and core k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> error_msgf "Retrieve invalid end of JSON array/object"
    | `Name _ -> error_msgf "Retrieve invalid JSON key value"
  and top () =
    match Jsonm.decode decoder with
    | #await as v -> await top v
    | #error as v -> error v
    | #eoi -> k `Null
    | `Lexeme (#Jsonm.lexeme as lexeme) -> core k lexeme
  in
  top ()

module Stack = struct
  type stack =
    | In_array of t list * stack
    | In_object of (string * t) list * stack
    | Empty
end

let encode ?minify ?(size_chunk = _max_young_size) ~output t =
  let encoder = Jsonm.encoder ?minify `Manual in
  let buf = Bytes.create size_chunk in
  let rec encode k stack value =
    match Jsonm.encode encoder value with
    | `Ok -> k stack
    | `Partial ->
        let len = Bytes.length buf - Jsonm.Manual.dst_rem encoder in
        output (Bytes.sub_string buf 0 len);
        Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
        encode k stack `Await
  and value k v stack =
    match v with
    | #value as v -> encode (continue k) stack (`Lexeme v)
    | `O ms -> encode (obj k ms) stack (`Lexeme `Os)
    | `A vs -> encode (arr k vs) stack (`Lexeme `As)
  and obj k ms stack =
    match ms with
    | (n, v) :: ms ->
        let stack = Stack.In_object (ms, stack) in
        encode (value k v) stack (`Lexeme (`Name n))
    | [] -> encode (continue k) stack (`Lexeme `Oe)
  and arr k vs stack =
    match vs with
    | v :: vs ->
        let stack = Stack.In_array (vs, stack) in
        value k v stack
    | [] -> encode (continue k) stack (`Lexeme `Ae)
  and continue k = function
    | Stack.In_array (vs, stack) -> arr k vs stack
    | Stack.In_object (ms, stack) -> obj k ms stack
    | Stack.Empty as stack -> encode k stack `End
  in
  Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
  value (Fun.const ()) t Stack.Empty
