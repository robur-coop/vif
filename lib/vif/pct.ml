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
