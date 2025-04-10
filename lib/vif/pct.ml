let src = Logs.Src.create "pct"

module Log = (val Logs.src_log src : Logs.LOG)

let safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-~"

let pchar =
  let arr = Array.make 256 false in
  for i = 0 to String.length safe - 1 do
    arr.(Char.code safe.[i]) <- true
  done;
  arr.(Char.code ':') <- true;
  arr.(Char.code '@') <- true;
  arr

let safe_host = pchar

let safe_path =
  let v = "!$&'()*+,;=" in
  let arr = Array.copy pchar in
  for i = 0 to String.length v - 1 do
    arr.(Char.code v.[i]) <- true
  done;
  arr.(Char.code '/') <- true;
  arr

let safe_query =
  let arr = Array.copy pchar in
  arr.(Char.code '/') <- true;
  arr.(Char.code '?') <- true;
  arr.(Char.code '&') <- false;
  arr.(Char.code ';') <- false;
  arr.(Char.code '+') <- false;
  arr

let safe_query_key =
  let arr = Array.copy safe_query in
  arr.(Char.code '=') <- false;
  arr

let safe_query_value =
  let arr = Array.copy safe_query in
  arr.(Char.code ',') <- false;
  arr

let encode safe_chars str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec scan start cur =
    if cur >= len then Buffer.add_substring buf str start (cur - start)
    else if safe_chars.(Char.code str.[cur]) then scan start (succ cur)
    else begin
      if cur > start then Buffer.add_substring buf str start (cur - start);
      Buffer.add_string buf (Format.asprintf "%%%02X" (Char.code str.[cur]));
      scan (succ cur) (succ cur)
    end
  in
  scan 0 0; Buffer.contents buf

let encode_path str = encode safe_path str
let encode_host str = encode safe_host str

let encode_query lst =
  let enc =
    List.map
      (fun (k, vs) ->
        let k' = encode safe_query_key k in
        let vs' = List.map (encode safe_query_value) vs in
        k' ^ "=" ^ String.concat "," vs')
      lst
  in
  match lst with _ :: _ -> "?" ^ String.concat "&" enc | [] -> ""

let int_of_hex chr =
  let code = int_of_char (Char.uppercase_ascii chr) - 48 in
  if code > 9 then
    if code > 16 && code < 23 then code - 7 else failwith "int_of_hex"
  else if code >= 0 then code
  else failwith "int_of_hex"

let plus_to_space str =
  let buf = Bytes.unsafe_of_string str in
  for i = 0 to Bytes.length buf - 1 do
    if Bytes.get buf i = '+' then Bytes.set buf i ' '
  done;
  Bytes.unsafe_to_string buf

let decode_pct str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec go start cur =
    if cur >= len then Buffer.add_substring buf str start (cur - start)
    else if str.[cur] = '%' then begin
      Buffer.add_substring buf str start (cur - start);
      let cur = cur + 1 in
      if cur >= len then Buffer.add_char buf '%'
      else
        match int_of_hex str.[cur] with
        | exception _ -> Buffer.add_char buf '%'; go cur cur
        | hi ->
            let cur = cur + 1 in
            if cur >= len then begin
              Buffer.add_char buf '%';
              Buffer.add_char buf str.[cur - 1]
            end
            else
              let start_at =
                match int_of_hex str.[cur] with
                | exception _ ->
                    Buffer.add_char buf '%';
                    Buffer.add_char buf str.[cur - 1];
                    cur
                | lo ->
                    Buffer.add_char buf (Char.chr ((hi lsl 4) + lo));
                    cur + 1
              in
              go start_at start_at
    end
    else go start (cur + 1)
  in
  go 0 0; Buffer.contents buf

let decode_query str =
  let split_query str =
    let rec go acc = function
      | (k, Some v) :: r ->
          let k = plus_to_space k in
          let v = plus_to_space v in
          let v = String.split_on_char ',' v in
          go ((k, v) :: acc) r
      | (k, None) :: r ->
          let k = plus_to_space k in
          go ((k, []) :: acc) r
      | [] -> acc
    in
    match String.split_on_char '&' str with
    | [] -> assert false
    | els ->
        let fn str =
          match String.split_on_char '=' str with
          | [] -> assert false
          | [ x ] -> (x, None)
          | k :: v :: r ->
              let v = String.concat "=" (v :: r) in
              (k, Some v)
        in
        let els = List.rev_map fn els in
        go [] els
  in
  let lst = split_query str in
  List.map (fun (k, v) -> (decode_pct k, List.map decode_pct v)) lst

let query_of_target str =
  match String.split_on_char '?' str with
  | [] | [ _ ] -> []
  | _ :: rest ->
      let str = String.concat "?" rest in
      Log.debug (fun m -> m "decode the query part: %s" str);
      decode_query str
