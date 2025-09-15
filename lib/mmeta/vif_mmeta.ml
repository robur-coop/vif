let src = Logs.Src.create "vif.meta"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

type t =
  | Node of { name: string; value: string; contents: t list }
    (* [name] "[value]" ( [contents] ),
       like [package "lib" ( ... )] *)
  | Set of { name: string; predicates: predicate list; value: string }
    (* [name] [(...) as predicates] = [value],
       like [archive(native) = "lib.cmxa"]*)
  | Add of { name: string; predicates: predicate list; value: string }
(* [name] [(...) as predicates] = [value],
   like [archive(native) += "lib.cmxa"]*)

and predicate = Include of string | Exclude of string

let pp_predicate ppf = function
  | Include p -> Format.pp_print_string ppf p
  | Exclude p -> Format.fprintf ppf "-%s" p

let rec pp ppf = function
  | Node { name; value; contents } ->
      Format.fprintf ppf "%s %S (@\n@[<2>%a@]@\n)" name value
        Fmt.(list ~sep:(any "@\n") pp)
        contents
  | Set { name; predicates= []; value } ->
      Format.fprintf ppf "%s = %S" name value
  | Set { name; predicates; value } ->
      Format.fprintf ppf "%s(%a) = %S" name
        Fmt.(list ~sep:(any ",") pp_predicate)
        predicates value
  | Add { name; predicates= []; value } ->
      Format.fprintf ppf "%s += %S" name value
  | Add { name; predicates; value } ->
      Format.fprintf ppf "%s(%a) += %S" name
        Fmt.(list ~sep:(any ",") pp_predicate)
        predicates value

module Assoc = struct
  type t = (string * string list) list

  let add k v t =
    match List.assoc_opt k t with
    | Some vs ->
        let vs = List.sort_uniq String.compare (v :: vs) in
        (k, vs) :: List.remove_assoc k t
    | None -> (k, [ v ]) :: t

  let set k v t =
    match List.assoc_opt k t with
    | Some _ -> (k, [ v ]) :: List.remove_assoc k t
    | None -> (k, [ v ]) :: t
end

module Path = struct
  type t = string list

  let of_string str =
    let str = String.trim str in
    let pkg = String.split_on_char '.' str in
    let rec go = function
      | [] -> Ok pkg
      | "" :: _ -> error_msgf "Invalid package name: %S" str
      | _ :: rest -> go rest
    in
    go pkg

  let of_string_exn str =
    match of_string str with
    | Ok pkg -> pkg
    | Error (`Msg msg) -> invalid_arg msg

  let pp ppf pkg = Format.fprintf ppf "%S" (String.concat "." pkg)
  let equal a b = try List.for_all2 String.equal a b with _ -> false
end

let compile ~predicates t ks =
  let incl ps =
    let one = function
      | Include p -> List.exists (String.equal p) predicates
      | Exclude p -> not (List.exists (String.equal p) predicates)
    in
    List.exists one ps
  in
  let find_directory contents =
    let rec go result = function
      | [] -> result
      | Add { name= "directory"; predicates= []; value } :: rest ->
          if Option.is_none result then go (Some value) rest else go result rest
      | Add { name= "directory"; predicates; value } :: rest ->
          if incl predicates && Option.is_none result then go (Some value) rest
          else go result rest
      | Set { name= "directory"; predicates= []; value } :: rest ->
          go (Some value) rest
      | Set { name= "directory"; predicates; value } :: rest ->
          if incl predicates then go (Some value) rest else go result rest
      | _ :: rest -> go result rest
    in
    go None contents
  in
  let rec go ~directory acc t = function
    | [] ->
        let rec go acc = function
          | [] ->
              let acc = List.remove_assoc "directory" acc in
              ("directory", [ directory ]) :: acc
          | Node _ :: rest -> go acc rest
          | Add { name; predicates= []; value } :: rest ->
              go (Assoc.add name value acc) rest
          | Set { name; predicates= []; value } :: rest ->
              go (Assoc.set name value acc) rest
          | Add { name; predicates; value } :: rest ->
              if incl predicates then go (Assoc.add name value acc) rest
              else go acc rest
          | Set { name; predicates; value } :: rest ->
              if incl predicates then go (Assoc.set name value acc) rest
              else go acc rest
        in
        go acc t
    | k :: ks -> begin
        match t with
        | [] -> acc
        | Node { name= "package"; value; contents } :: rest ->
            let directory' =
              match find_directory contents with
              | Some v -> Filename.concat directory v
              | None -> directory
            in
            if k = value then go ~directory:directory' acc contents ks
            else go ~directory acc rest (k :: ks)
        | _ :: rest -> go ~directory acc rest (k :: ks)
      end
  in
  go ~directory:"" [] t ks

exception Parser_error of string

let raise_parser_error lexbuf fmt =
  let p = Lexing.lexeme_start_p lexbuf in
  let c = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Format.kasprintf
    (fun msg -> raise (Parser_error msg))
    ("%s (l.%d c.%d): " ^^ fmt)
    p.Lexing.pos_fname p.Lexing.pos_lnum c

let pp_token ppf = function
  | Vif_mmeta_lexer.Name name -> Format.pp_print_string ppf name
  | String str -> Format.fprintf ppf "%S" str
  | Minus -> Format.pp_print_string ppf "-"
  | Lparen -> Format.pp_print_string ppf "("
  | Rparen -> Format.pp_print_string ppf ")"
  | Comma -> Format.pp_print_string ppf ","
  | Equal -> Format.pp_print_string ppf "="
  | Plus_equal -> Format.pp_print_string ppf "+="
  | Eof -> Format.pp_print_string ppf "#eof"

let invalid_token lexbuf token =
  raise_parser_error lexbuf "Invalid token %a" pp_token token

let lparen lexbuf =
  match Vif_mmeta_lexer.token lexbuf with
  | Lparen -> ()
  | token -> invalid_token lexbuf token

let name lexbuf =
  match Vif_mmeta_lexer.token lexbuf with
  | Name name -> name
  | token -> invalid_token lexbuf token

let string lexbuf =
  match Vif_mmeta_lexer.token lexbuf with
  | String str -> str
  | token -> invalid_token lexbuf token

let rec predicates lexbuf acc =
  match Vif_mmeta_lexer.token lexbuf with
  | Rparen -> List.rev acc
  | Name predicate -> begin
      match Vif_mmeta_lexer.token lexbuf with
      | Comma -> predicates lexbuf (Include predicate :: acc)
      | Rparen -> List.rev (Include predicate :: acc)
      | token -> invalid_token lexbuf token
    end
  | Minus ->
      let predicate = name lexbuf in
      begin
        match Vif_mmeta_lexer.token lexbuf with
        | Comma -> predicates lexbuf (Exclude predicate :: acc)
        | Rparen -> List.rev (Exclude predicate :: acc)
        | token -> invalid_token lexbuf token
      end
  | token -> invalid_token lexbuf token

let rec parser lexbuf depth acc =
  match Vif_mmeta_lexer.token lexbuf with
  | Rparen when depth > 0 -> List.rev acc
  | Rparen ->
      raise_parser_error lexbuf
        "Closing parenthesis without matching opening one"
  | Eof when depth = 0 -> List.rev acc
  | Eof -> raise_parser_error lexbuf "%d closing parenthesis missing" depth
  | Name name -> begin
      match Vif_mmeta_lexer.token lexbuf with
      | String value ->
          lparen lexbuf;
          let contents = parser lexbuf (succ depth) [] in
          parser lexbuf depth (Node { name; value; contents } :: acc)
      | Equal ->
          let value = string lexbuf in
          parser lexbuf depth (Set { name; predicates= []; value } :: acc)
      | Plus_equal ->
          let value = string lexbuf in
          parser lexbuf depth (Add { name; predicates= []; value } :: acc)
      | Lparen ->
          let predicates = predicates lexbuf [] in
          begin
            match Vif_mmeta_lexer.token lexbuf with
            | Equal ->
                let value = string lexbuf in
                parser lexbuf depth (Set { name; predicates; value } :: acc)
            | Plus_equal ->
                let value = string lexbuf in
                parser lexbuf depth (Add { name; predicates; value } :: acc)
            | token -> invalid_token lexbuf token
          end
      | token -> invalid_token lexbuf token
    end
  | token -> invalid_token lexbuf token

let parser lexbuf =
  try Ok (parser lexbuf 0 []) with
  | Parser_error err -> Error (`Msg err)
  | Vif_mmeta_lexer.Lexical_error (msg, f, l, c) ->
      error_msgf "%s at l.%d, c.%d: %s" f l c msg

let parser path =
  let ( let@ ) finally fn = Fun.protect ~finally fn in
  let ic = open_in path in
  let@ _ = fun () -> close_in ic in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf path;
  parser lexbuf

let rec incl us vs =
  match (us, vs) with
  | u :: us, v :: vs -> if u = v then incl us vs else false
  | [], _ | _, [] -> true

let rec diff us vs =
  match (us, vs) with
  | u :: us, v :: vs ->
      if u = v then diff us vs else error_msgf "Different paths (%S <> %S)" u v
  | [], x | x, [] -> Ok x

let is_prefix ~prefix path =
  if not (String.starts_with ~prefix path) then false
  else
    let suff_start = String.length prefix in
    prefix.[suff_start - 1] = Filename.dir_sep.[0]
    || suff_start = String.length path
    || path.[suff_start] = Filename.dir_sep.[0]

let segs_to_path segs = String.concat Filename.dir_sep segs
let segs_of_path = String.split_on_char Filename.dir_sep.[0]

let rem_empty_seg p =
  match String.length p with
  | 1 -> p
  | 2 ->
      if p.[0] <> Filename.dir_sep.[0] && p.[1] = Filename.dir_sep.[0] then
        String.make 1 p.[0]
      else p
  | len ->
      let max = len - 1 in
      if p.[max] <> Filename.dir_sep.[0] then p else String.sub p 0 (max - 1)

let to_dir_path location =
  if Filename.check_suffix location "/" then location else location ^ "/"

let is_dir_path = Filename.check_suffix Filename.dir_sep

let relativize ~root p =
  if String.equal root p then
    Some (segs_to_path (if is_dir_path p then [ "."; "" ] else [ "." ]))
  else
    let root =
      if
        String.length root > 0
        && root.[String.length root - 1] = Filename.dir_sep.[0]
      then root
      else root ^ Filename.dir_sep
    in
    let rec go root p =
      match (root, p) with
      | ".." :: _, s :: _ when s <> ".." -> None
      | sr :: root, sp :: (_ :: _ as p) when sr = sp -> go root p
      | [ "" ], [ "" ] -> Some (segs_to_path [ "."; "" ])
      | root, p ->
          let segs =
            List.fold_left (fun acc _ -> ".." :: acc) p (List.tl root)
          in
          Some (segs_to_path segs)
    in
    match (segs_of_path root, segs_of_path p) with
    | "" :: _, s :: _ when s <> "" -> None
    | s :: _, "" :: _ when s <> "" -> None
    | [ "."; "" ], p -> Some (segs_to_path p)
    | root, p -> go root p

let relativize ~roots path =
  let rec go = function
    | [] -> assert false
    | root :: roots ->
        if is_prefix ~prefix:root path then
          match relativize ~root path with
          | Some rel -> (root, rel)
          | None -> go roots
        else go roots
  in
  go roots

let ( / ) = Filename.concat

module Map = Map.Make (String)

let search ~roots ?(predicates = [ "native"; "byte" ]) meta_path =
  let ( >>= ) = Result.bind in
  let ( >>| ) x f = Result.map f x in
  let elements path =
    let path = Fpath.to_string path in
    if Sys.is_directory path then Ok false
    else if Filename.basename path = "META" then Ok true
    else Ok false
  in
  let traverse path =
    let path = Fpath.to_string path in
    if List.exists (String.equal path) roots then Ok true
    else begin
      let _, rel = relativize ~roots path in
      let meta_path' = segs_of_path rel in
      Ok (incl meta_path meta_path')
    end
  in
  let fold path acc =
    let path = Fpath.to_string path in
    let root, rel = relativize ~roots path in
    let package = rem_empty_seg (Filename.dirname rel) in
    let meta_path' = segs_of_path package in
    match
      diff meta_path meta_path' >>= fun ks ->
      parser path >>| fun meta -> compile ~predicates meta ks
    with
    | Ok descr ->
        Log.debug (fun m -> m "add %s" (root / Filename.dirname rel));
        Log.debug (fun m ->
            m "%a" Fmt.(Dump.list (Dump.pair string (Dump.list string))) descr);
        Map.add (root / Filename.dirname rel) descr acc
    | Error (`Msg msg) ->
        Log.warn (fun m ->
            m "Impossible to extract the META file of %s: %s" path msg);
        acc
  in
  let err _path _ = Ok () in
  Bos.OS.Path.fold ~err ~dotfiles:false ~elements:(`Sat elements)
    ~traverse:(`Sat traverse) fold Map.empty (List.map Fpath.v roots)
  >>| Map.bindings

let dependencies_of (_path, descr) =
  Stdlib.Option.value ~default:[] (List.assoc_opt "requires" descr)
  |> List.map (Astring.String.cuts ~empty:false ~sep:" ")
  |> List.concat
  |> List.map Path.of_string_exn

exception Cycle

let get_dep (_, path, descr) graph =
  let deps = dependencies_of (path, descr) in
  List.map
    (fun name -> List.find (fun (name', _, _) -> Path.equal name name') graph)
    deps

type graph = (Path.t * string * Assoc.t) list

let dfs (graph : graph) visited start_node =
  let rec explore path visited node =
    if List.mem node path then raise Cycle
    else if List.mem node visited then visited
    else
      let new_path = node :: path in
      let edges = get_dep node graph in
      let visited = List.fold_left (explore new_path) visited edges in
      node :: visited
  in
  explore [] visited start_node

let sort graph =
  List.fold_left (fun visited node -> dfs graph visited node) [] graph

let ancestors ~roots ?(predicates = [ "native"; "byte" ]) meta_path =
  let rec go acc visited = function
    | [] -> Ok acc
    | meta_path :: todo when List.mem meta_path visited -> go acc visited todo
    | meta_path :: todo -> (
        match search ~roots ~predicates meta_path with
        | Ok pkgs ->
            let requires = List.concat (List.map dependencies_of pkgs) in
            let pkgs =
              List.map (fun (path, descr) -> (meta_path, path, descr)) pkgs
            in
            go (List.rev_append pkgs acc) (meta_path :: visited)
              (List.rev_append requires todo)
        | Error _ as err -> err)
  in
  let open Rresult in
  go [] [] [ meta_path ] >>| fun lst -> sort lst |> List.rev

let to_artifacts pkgs =
  let ( let* ) = Result.bind in
  let fn acc (path, pkg) =
    match acc with
    | Error _ as err -> err
    | Ok acc ->
        let directory = List.assoc_opt "directory" pkg in
        let* directory =
          match directory with
          | Some [ dir ] -> Ok (path / dir)
          | Some _ ->
              error_msgf "Multiple directories referenced by %s" (path / "META")
          | None -> Ok path
        in
        let directory = to_dir_path directory in
        let archive = List.assoc_opt "archive" pkg in
        let archive = Stdlib.Option.value ~default:[] archive in
        let plugin = List.assoc_opt "plugin" pkg in
        let plugin = Stdlib.Option.value ~default:[] plugin in
        let archive = List.map (( / ) directory) archive in
        let plugin = List.map (( / ) directory) plugin in
        Ok List.(rev_append archive (rev_append plugin acc))
  in
  let* paths = List.fold_left fn (Ok []) pkgs in
  Objinfo.vs paths

open Cmdliner

let directories =
  let doc = "The source directory containing the META files." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str && Sys.is_directory str -> v
    | Ok v -> error_msgf "%a is not a directory or does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let open Arg in
  value
  & opt_all (conv (parser, Fpath.pp)) []
  & info [ "I" ] ~doc ~docv:"DIRECTORY"

let setup user's_directories =
  let cmd = Bos.Cmd.(v "ocamlfind" % "printconf" % "path") in
  let ( let* ) = Result.bind in
  let directories =
    let* exists = Bos.OS.Cmd.exists cmd in
    if exists then
      let r = Bos.OS.Cmd.run_out cmd in
      let* directories, _ = Bos.OS.Cmd.out_lines ~trim:true r in
      let directories =
        List.fold_left
          (fun acc path ->
            match Fpath.of_string path with
            | Ok path -> path :: acc
            | Error (`Msg _) ->
                Logs.warn (fun m ->
                    m "ocamlfind returned an invalid path: %S" path);
                acc)
          [] directories
      in
      Ok directories
    else Ok []
  in
  let directories = Result.value ~default:[] directories in
  List.rev_append directories user's_directories

let setup = Term.(const setup $ directories)
