let src = Logs.Src.create "vif.stream"

module Log = (val Logs.src_log src : Logs.LOG)

let ( % ) f g = fun x -> f (g x)

module Bqueue = Multipart_form_miou.Bounded_stream

type 'a source =
  | Source : {
        init: unit -> 's
      ; pull: 's -> ('a * 's) option
      ; stop: 's -> unit
    }
      -> 'a source

module Source = struct
  let file ?offset path =
    let buf = Bytes.create 0x7ff in
    let init () =
      let fd = Unix.openfile path Unix.[ O_RDONLY ] 0o644 in
      match offset with
      | Some offset ->
          let _ = Unix.lseek fd offset Unix.SEEK_SET in
          fd
      | None -> fd
    in
    let stop fd = Unix.close fd in
    let pull fd =
      let len = Unix.read fd buf 0 (Bytes.length buf) in
      if len == 0 then None else Some (Bytes.sub_string buf 0 len, fd)
    in
    (Source { init; stop; pull } : string source)

  let list lst =
    let pull = function [] -> None | x :: r -> Some (x, r) in
    Source { init= Fun.const lst; pull; stop= ignore }

  let dispose (Source src) = src.stop (src.init ())

  let with_formatter fn =
    let bqueue = Bqueue.create 0x100 in
    let out str off len = Bqueue.put bqueue (String.sub str off len) in
    let ppf = Format.make_formatter out ignore in
    let init () = Miou.async @@ fun () -> fn ppf; Bqueue.close bqueue in
    let pull prm =
      match Bqueue.get bqueue with
      | Some str -> Some (str, prm)
      | None -> Miou.await_exn prm; None
    in
    let stop prm = Miou.await_exn prm in
    Source { init; pull; stop }

  let with_task ~limit fn =
    let bqueue = Bqueue.create limit in
    let init () = Miou.async @@ fun () -> fn bqueue in
    let pull prm =
      match Bqueue.get bqueue with
      | Some a -> Some (a, prm)
      | None -> Miou.await_exn prm; None
    in
    let stop prm = Miou.await_exn prm in
    Source { init; pull; stop }

  let of_bqueue bqueue =
    let init () = bqueue in
    let pull bqueue =
      match Bqueue.get bqueue with Some a -> Some (a, bqueue) | None -> None
    in
    let stop _bqueue = () in
    Source { init; pull; stop }

  let to_reader (Source { init; pull; _ }) =
    let src = ref (init ()) in
    let rec fn () =
      match pull !src with
      | Some ("", src') ->
          src := src';
          (fn [@tailcall]) ()
      | Some (str, src') ->
          src := src';
          let first = 0 and length = String.length str in
          Bytesrw.Bytes.Slice.make (Bytes.of_string str) ~first ~length
      | None -> Bytesrw.Bytes.Slice.eod
    in
    Bytesrw.Bytes.Reader.make fn
end

type ('a, 'r) sink =
  | Sink : {
        init: unit -> 's
      ; push: 's -> 'a -> 's
      ; full: 's -> bool
      ; stop: 's -> 'r
    }
      -> ('a, 'r) sink

module Bstr = struct
  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
  external get_int32_ne : bigstring -> int -> int32 = "%caml_bigstring_get32"

  external unsafe_set_int32_ne : bigstring -> int -> int32 -> unit
    = "%caml_bigstring_set32u"

  external unsafe_set_uint8 : bigstring -> int -> int -> unit
    = "%caml_ba_unsafe_set_1"

  let blit_to_bytes bstr ~src_off dst ~dst_off ~len =
    if
      len < 0
      || src_off < 0
      || src_off > Bigarray.Array1.dim bstr - len
      || dst_off < 0
      || dst_off > Bytes.length dst - len
    then invalid_arg "Bstr.blit_to_bytes";
    let len0 = len land 3 in
    let len1 = len lsr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = get_int32_ne bstr (src_off + i) in
      Bytes.set_int32_ne dst (dst_off + i) v
    done;
    for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = get_uint8 bstr (src_off + i) in
      Bytes.set_uint8 dst (dst_off + i) v
    done

  let blit_from_bytes src ~src_off bstr ~dst_off ~len =
    if
      len < 0
      || src_off < 0
      || src_off > Bytes.length src - len
      || dst_off < 0
      || dst_off > Bigarray.Array1.dim bstr - len
    then invalid_arg "Bstr.blit_from_bytes";
    let len0 = len land 3 in
    let len1 = len lsr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = Bytes.get_int32_ne src (src_off + i) in
      unsafe_set_int32_ne bstr (dst_off + i) v
    done;
    for i = 0 to len0 - 1 do
      let i = (len1 * 4) + i in
      let v = Bytes.get_uint8 src (src_off + i) in
      unsafe_set_uint8 bstr (dst_off + i) v
    done

  let sub_string bstr ~off ~len =
    let buf = Bytes.create len in
    blit_to_bytes bstr ~src_off:off buf ~dst_off:0 ~len;
    Bytes.unsafe_to_string buf

  let of_string str =
    let len = String.length str in
    let bstr = Bigarray.(Array1.create char c_layout len) in
    blit_from_bytes (Bytes.unsafe_of_string str) ~src_off:0 bstr ~dst_off:0 ~len;
    bstr
end

module Sink = struct
  type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
  type await = [ `Await ]
  type error = [ `Error of Jsonm.error ]
  type eoi = [ `End ]

  let errorf fmt = Fmt.kstr (fun msg -> `Error msg) fmt

  let string =
    let init () = Buffer.create 0x7ff in
    let push buf str = Buffer.add_string buf str; buf in
    let full = Fun.const false in
    let stop = Buffer.contents in
    Sink { init; push; full; stop }

  let list =
    let init () = [] in
    let push acc x = x :: acc in
    let full _ = false in
    let stop acc = List.rev acc in
    Sink { init; push; full; stop }

  let json () =
    let decoder = Jsonm.decoder `Manual in
    let rec error (`Error err) =
      errorf "Invalid JSON input: %a" Jsonm.pp_error err
    and end_of_input `End = errorf "Unexpected end of input"
    and arr acc k =
      match Jsonm.decode decoder with
      | #await -> `Await (fun () -> arr acc k)
      | #error as v -> error v
      | #eoi as v -> end_of_input v
      | `Lexeme `Ae -> k (`A (List.rev acc))
      | `Lexeme v -> core (fun v -> arr (v :: acc) k) v
    and name n k =
      match Jsonm.decode decoder with
      | #await -> `Await (fun () -> name n k)
      | #error as v -> error v
      | #eoi as v -> end_of_input v
      | `Lexeme v -> core (fun v -> k (n, v)) v
    and obj acc k =
      match Jsonm.decode decoder with
      | #await -> `Await (fun () -> obj acc k)
      | #error as v -> error v
      | #eoi as v -> end_of_input v
      | `Lexeme `Oe -> k (`O (List.rev acc))
      | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
      | `Lexeme v ->
          errorf "Unexpected lexeme: %a (expected key)" Jsonm.pp_lexeme v
    and core k = function
      | #value as v -> k v
      | `Os -> obj [] k
      | `As -> arr [] k
      | `Ae | `Oe -> errorf "Retrieve invalid end of JSON array/object"
      | `Name _ -> errorf "Retrieve invalid JSON key value"
    and init () =
      match Jsonm.decode decoder with
      | #await -> `Await init
      | #error as v -> error v
      | #eoi -> `Json `Null
      | `Lexeme (#Jsonm.lexeme as lexeme) -> core (fun v -> `Json v) lexeme
    in
    let push v str =
      match v with
      | `Await k ->
          Jsonm.Manual.src decoder
            (Bytes.unsafe_of_string str)
            0 (String.length str);
          k ()
      | `Error _ as err -> err
      | `Json _ as value -> value
    in
    let full = function `Error _ | `Json _ -> true | _ -> false in
    let rec stop = function
      | `Await k ->
          Jsonm.Manual.src decoder Bytes.empty 0 0;
          stop (k ())
      | `Error msg -> Error (`Msg msg)
      | `Json value -> Ok value
    in
    Sink { init; push; full; stop }

  let file filename =
    let init () = lazy (Stdlib.open_out_bin filename) in
    let stop oc = if Lazy.is_val oc then close_out (Lazy.force oc) in
    let push oc str =
      let ch = Lazy.force oc in
      Logs.debug (fun m -> m "save %d byte(s)" (String.length str));
      Stdlib.output_string ch str;
      Stdlib.flush ch;
      oc
    in
    let full _ = false in
    Sink { init; stop; full; push }

  let drain =
    let init () = () in
    let push () _ = () in
    let full () = false in
    let stop () = () in
    Sink { init; push; full; stop }

  let each fn =
    let rec terminate ?exn orphans =
      match (Miou.care orphans, exn) with
      | None, None -> Ok orphans
      | None, Some exn -> Error exn
      | Some None, _ -> Miou.yield (); terminate ?exn orphans
      | Some (Some prm), _ -> (
          match (Miou.await prm, exn) with
          | Ok (), _ -> terminate ?exn orphans
          | Error exn, None -> terminate ~exn orphans
          | Error _, _ -> terminate ?exn orphans)
    in
    let rec clean orphans =
      match Miou.care orphans with
      | None | Some None -> Ok orphans
      | Some (Some prm) -> (
          match Miou.await prm with
          | Ok () -> clean orphans
          | Error exn -> terminate ~exn orphans)
    in
    let init () = Ok (Miou.orphans ()) in
    let push value x =
      match Result.bind value clean with
      | Ok orphans ->
          ignore (Miou.async ~orphans @@ fun () -> fn x);
          Ok orphans
      | Error _ as err -> err
    in
    let full = Result.is_error in
    let stop value =
      match Result.bind value terminate with
      | Ok _orphans -> ()
      | Error exn -> raise exn
    in
    Sink { init; stop; full; push }
end

type ('a, 'b) flow = { flow: 'r. ('b, 'r) sink -> ('a, 'r) sink } [@@unboxed]

module Flow = struct
  let identity = { flow= Fun.id }
  let compose { flow= f } { flow= g } = { flow= (fun sink -> f (g sink)) }
  let ( << ) a b = compose a b
  let ( >> ) b a = compose a b

  let map fn =
    let flow (Sink k) =
      let push r x = k.push r (fn x) in
      Sink { k with push }
    in
    { flow }

  let rec deflate_until_end ~push ~acc encoder o =
    match Zl.Def.encode encoder with
    | `Await _ -> assert false
    | `Flush encoder ->
        let len = Bigarray.Array1.dim o - Zl.Def.dst_rem encoder in
        let encoder = Zl.Def.dst encoder o 0 (Bigarray.Array1.dim o) in
        let acc = push acc (Bstr.sub_string o ~off:0 ~len) in
        deflate_until_end ~push ~acc encoder o
    | `End encoder ->
        let len = Bigarray.Array1.dim o - Zl.Def.dst_rem encoder in
        push acc (Bstr.sub_string o ~off:0 ~len)

  let rec deflate_until_await ~push ~acc encoder o =
    match Zl.Def.encode encoder with
    | `Await encoder -> (encoder, o, acc)
    | `Flush encoder ->
        let len = Bigarray.Array1.dim o - Zl.Def.dst_rem encoder in
        let encoder = Zl.Def.dst encoder o 0 (Bigarray.Array1.dim o) in
        let acc = push acc (Bstr.sub_string o ~off:0 ~len) in
        deflate_until_await ~push ~acc encoder o
    | `End _ -> assert false

  let deflate ?(q = De.Queue.create 0x100) ?(w = De.Lz77.make_window ~bits:15)
      ?(level = 4) () =
    let flow (Sink k) =
      let init () =
        let encoder = Zl.Def.encoder ~q ~w ~level `Manual `Manual in
        let o = De.bigstring_create 0x7ff in
        let encoder = Zl.Def.dst encoder o 0 0x7ff in
        let acc = k.init () in
        (encoder, o, acc)
      in
      let push (encoder, o, acc) = function
        | "" -> (encoder, o, acc)
        | str ->
            let bstr = Bstr.of_string str in
            let encoder = Zl.Def.src encoder bstr 0 (String.length str) in
            deflate_until_await ~push:k.push ~acc encoder o
      in
      let full (_, _, acc) = k.full acc in
      let stop (encoder, o, acc) =
        let encoder = Zl.Def.src encoder De.bigstring_empty 0 0 in
        let acc = deflate_until_end ~push:k.push ~acc encoder o in
        k.stop acc
      in
      Sink { init; stop; full; push }
    in
    { flow }

  let rec gzip_until_end ~push ~acc encoder o =
    match Gz.Def.encode encoder with
    | `Await _ -> assert false
    | `Flush encoder ->
        let len = Bigarray.Array1.dim o - Gz.Def.dst_rem encoder in
        let encoder = Gz.Def.dst encoder o 0 (Bigarray.Array1.dim o) in
        let acc = push acc (Bstr.sub_string o ~off:0 ~len) in
        gzip_until_end ~push ~acc encoder o
    | `End encoder ->
        let len = Bigarray.Array1.dim o - Gz.Def.dst_rem encoder in
        push acc (Bstr.sub_string o ~off:0 ~len)

  let rec gzip_until_await ~push ~acc encoder o =
    match Gz.Def.encode encoder with
    | `Await encoder -> (encoder, o, acc)
    | `Flush encoder ->
        let len = Bigarray.Array1.dim o - Gz.Def.dst_rem encoder in
        let encoder = Gz.Def.dst encoder o 0 (Bigarray.Array1.dim o) in
        let acc = push acc (Bstr.sub_string o ~off:0 ~len) in
        gzip_until_await ~push ~acc encoder o
    | `End _ -> assert false

  let now () = Int32.of_float (Unix.gettimeofday ())

  let gzip ?(q = De.Queue.create 0x100) ?(w = De.Lz77.make_window ~bits:15)
      ?(level = 4) () =
    let flow (Sink k) =
      let init () =
        let encoder =
          Gz.Def.encoder ~q ~w ~level ~mtime:(now ()) `Manual `Manual Gz.Unix
        in
        let o = De.bigstring_create 0x7ff in
        let encoder = Gz.Def.dst encoder o 0 0x7ff in
        let acc = k.init () in
        (encoder, o, acc)
      in
      let push (encoder, o, acc) = function
        | "" -> (encoder, o, acc)
        | str ->
            let bstr = Bstr.of_string str in
            let encoder = Gz.Def.src encoder bstr 0 (String.length str) in
            gzip_until_await ~push:k.push ~acc encoder o
      in
      let full (_, _, acc) = k.full acc in
      let stop (encoder, o, acc) =
        let encoder = Gz.Def.src encoder De.bigstring_empty 0 0 in
        let acc = gzip_until_end ~push:k.push ~acc encoder o in
        k.stop acc
      in
      Sink { init; stop; full; push }
    in
    { flow }

  let consume bqueue push acc =
    let rec go acc () =
      match Bqueue.get bqueue with None -> acc | Some a -> go (push acc a) ()
    in
    go acc

  let bound limit =
    let flow (Sink k) =
      let init () =
        let bqueue = Bqueue.create limit in
        let acc = k.init () in
        let prm = Miou.async (consume bqueue k.push acc) in
        (bqueue, prm)
      in
      let push (bqueue, prm) a = Bqueue.put bqueue a; (bqueue, prm) in
      let full _ = false in
      let stop (bqueue, prm) =
        Bqueue.close bqueue;
        let acc = Miou.await_exn prm in
        k.stop acc
      in
      Sink { init; stop; full; push }
    in
    { flow }
end

external reraise : exn -> 'a = "%reraise"

type 'a stream = { stream: 'r. ('a, 'r) sink -> 'r } [@@unboxed]

module Stream = struct
  let run ~from:(Source src) ~via:{ flow } ~into:snk =
    let (Sink snk) = flow snk in
    let rec loop r s =
      match snk.full r with
      | true ->
          Log.debug (fun m -> m "our sink is full");
          let r' = snk.stop r in
          let leftover = Source { src with init= Fun.const s } in
          (r', Some leftover)
      | false -> begin
          match src.pull s with
          | Some (x, s') -> loop (snk.push r x) s'
          | None ->
              src.stop s;
              Log.debug (fun m -> m "stop our sink, source is empty");
              let r' = snk.stop r in
              (r', None)
        end
    in
    let r0 = snk.init () in
    match snk.full r0 with
    | true ->
        Log.debug (fun m -> m "dispose our source, our sink is full");
        let r' = snk.stop r0 in
        (r', Some (Source src))
    | false -> (
        let s0' = ref None in
        try
          let s0 = src.init () in
          s0' := Some s0;
          Log.debug (fun m ->
              m "start to consume our source and transmit it into our sink");
          loop r0 s0
        with exn ->
          Log.err (fun m ->
              m "Unexpected exception from our source: %s"
                (Printexc.to_string exn));
          Option.iter src.stop !s0';
          let _ = snk.stop r0 in
          reraise exn)

  let into sink t = t.stream sink

  let via { flow } t =
    let stream sink = into (flow sink) t in
    { stream }

  let from (Source src) =
    let stream (Sink k) =
      let rec go r s =
        if k.full r then k.stop r
        else
          match src.pull s with
          | None -> src.stop s; k.stop r
          | Some (x, s') -> go (k.push r x) s'
      in
      let r0 = k.init () in
      if k.full r0 then k.stop r0
      else
        let s0' = ref None in
        try
          let s0 = src.init () in
          s0' := Some s0;
          go r0 s0
        with exn ->
          Option.iter src.stop !s0';
          let _ = k.stop r0 in
          reraise exn
    in
    { stream }

  let map fn t = via (Flow.map fn) t
  let to_file filename = into (Sink.file filename)
  let drain t = into Sink.drain t
  let each fn t = into (Sink.each fn) t

  let bracket : init:(unit -> 's) -> stop:('s -> 'r) -> ('s -> 's) -> 'r =
   fun ~init ~stop fn ->
    let acc = init () in
    try stop (fn acc)
    with exn ->
      Log.err (fun m -> m "Stram.Sink.bracket: %s" (Printexc.to_string exn));
      let _ = stop acc in
      reraise exn

  let of_bqueue bq =
    let stream (Sink k) =
      let rec go r =
        if k.full r then r
        else
          let value = Bqueue.get bq in
          Option.fold ~none:r ~some:(go % k.push r) value
      in
      let stop r = k.stop r in
      bracket go ~init:k.init ~stop
    in
    { stream }

  let flat_map fn t =
    let stream (Sink k) =
      let push r x =
        (fn x).stream (Sink { k with init= Fun.const r; stop= Fun.id })
      in
      t.stream (Sink { k with push })
    in
    { stream }
end
