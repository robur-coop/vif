let src = Logs.Src.create "uniq.meta"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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
  | Include p -> Fmt.string ppf p
  | Exclude p -> Fmt.pf ppf "-%s" p

let rec pp ppf = function
  | Node { name; value; contents } ->
      Fmt.pf ppf "%s %S (@\n@[<2>%a@]@\n)" name value
        Fmt.(list ~sep:(any "@\n") pp)
        contents
  | Set { name; predicates= []; value } -> Fmt.pf ppf "%s = %S" name value
  | Set { name; predicates; value } ->
      Fmt.pf ppf "%s(%a) = %S" name
        Fmt.(list ~sep:(any ",") pp_predicate)
        predicates value
  | Add { name; predicates= []; value } -> Fmt.pf ppf "%s += %S" name value
  | Add { name; predicates; value } ->
      Fmt.pf ppf "%s(%a) += %S" name
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

  let pp ppf pkg = Fmt.string ppf (String.concat "." pkg)
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
  let rec go acc t = function
    | [] ->
        let rec go acc = function
          | [] -> acc
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
    | k :: ks -> (
        match t with
        | [] -> acc
        | Node { name= "package"; value; contents } :: rest ->
            if k = value then go acc contents ks else go acc rest (k :: ks)
        | _ :: rest -> go acc rest (k :: ks))
  in
  go [] t ks

exception Parser_error of string

let raise_parser_error lexbuf fmt =
  let p = Lexing.lexeme_start_p lexbuf in
  let c = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Fmt.kstr
    (fun msg -> raise (Parser_error msg))
    ("%s (l.%d c.%d): " ^^ fmt)
    p.Lexing.pos_fname p.Lexing.pos_lnum c

let pp_token ppf = function
  | Vif_meta_lexer.Name name -> Fmt.string ppf name
  | String str -> Fmt.pf ppf "%S" str
  | Minus -> Fmt.string ppf "-"
  | Lparen -> Fmt.string ppf "("
  | Rparen -> Fmt.string ppf ")"
  | Comma -> Fmt.string ppf ","
  | Equal -> Fmt.string ppf "="
  | Plus_equal -> Fmt.string ppf "+="
  | Eof -> Fmt.string ppf "#eof"

let invalid_token lexbuf token =
  raise_parser_error lexbuf "Invalid token %a" pp_token token

let lparen lexbuf =
  match Vif_meta_lexer.token lexbuf with
  | Lparen -> ()
  | token -> invalid_token lexbuf token

let name lexbuf =
  match Vif_meta_lexer.token lexbuf with
  | Name name -> name
  | token -> invalid_token lexbuf token

let string lexbuf =
  match Vif_meta_lexer.token lexbuf with
  | String str -> str
  | token -> invalid_token lexbuf token

let rec predicates lexbuf acc =
  match Vif_meta_lexer.token lexbuf with
  | Rparen -> List.rev acc
  | Name predicate -> begin
      match Vif_meta_lexer.token lexbuf with
      | Comma -> predicates lexbuf (Include predicate :: acc)
      | Rparen -> List.rev (Include predicate :: acc)
      | token -> invalid_token lexbuf token
    end
  | Minus ->
      let predicate = name lexbuf in
      begin
        match Vif_meta_lexer.token lexbuf with
        | Comma -> predicates lexbuf (Exclude predicate :: acc)
        | Rparen -> List.rev (Exclude predicate :: acc)
        | token -> invalid_token lexbuf token
      end
  | token -> invalid_token lexbuf token

let rec parser lexbuf depth acc =
  match Vif_meta_lexer.token lexbuf with
  | Rparen when depth > 0 -> List.rev acc
  | Rparen ->
      raise_parser_error lexbuf
        "Closing parenthesis without matching opening one"
  | Eof when depth = 0 -> List.rev acc
  | Eof -> raise_parser_error lexbuf "%d closing parenthesis missing" depth
  | Name name -> begin
      match Vif_meta_lexer.token lexbuf with
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
            match Vif_meta_lexer.token lexbuf with
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

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let parser lexbuf =
  try Ok (parser lexbuf 0 []) with
  | Parser_error err -> Error (`Msg err)
  | Vif_meta_lexer.Lexical_error (msg, f, l, c) ->
      error_msgf "%s at l.%d, c.%d: %s" f l c msg

let parser path =
  Log.debug (fun m -> m "Parse %a" Fpath.pp path);
  let ( let@ ) finally fn = Fun.protect ~finally fn in
  let ic = open_in (Fpath.to_string path) in
  let@ _ = fun () -> close_in ic in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf (Fpath.to_string path);
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

let relativize ~roots path =
  let rec go = function
    | [] -> assert false
    | root :: roots ->
        if Fpath.is_prefix root path then
          match Fpath.relativize ~root path with
          | Some rel -> (root, rel)
          | None -> go roots
        else go roots
  in
  go roots

let search ~roots ?(predicates = [ "native"; "byte" ]) meta_path =
  let ( >>= ) = Result.bind in
  let ( >>| ) x f = Result.map f x in
  let elements path =
    if Sys.is_directory (Fpath.to_string path) then Ok false
    else if Fpath.basename path = "META" then Ok true
    else Ok false
  in
  let traverse path =
    if List.exists (Fpath.equal path) roots then Ok true
    else begin
      let _, rel = relativize ~roots path in
      let meta_path' = Fpath.segs rel in
      Ok (incl meta_path meta_path')
    end
  in
  let fold path acc =
    let root, rel = relativize ~roots path in
    let package = Fpath.(rem_empty_seg (parent rel)) in
    let meta_path' = Fpath.(segs package) in
    match
      diff meta_path meta_path' >>= fun ks ->
      parser path >>| fun meta -> compile ~predicates meta ks
    with
    | Ok descr -> Fpath.Map.add Fpath.(root // parent rel) descr acc
    | Error (`Msg msg) ->
        Log.warn (fun m ->
            m "Impossible to extract the META file of %a: %s" Fpath.pp path msg);
        acc
  in
  let err _path _ = Ok () in
  Bos.OS.Path.fold ~err ~dotfiles:false ~elements:(`Sat elements)
    ~traverse:(`Sat traverse) fold Fpath.Map.empty roots
  >>| Fpath.Map.bindings

let dependencies_of (_path, descr) =
  Stdlib.Option.value ~default:[] (List.assoc_opt "requires" descr)
  |> List.map (Astring.String.cuts ~empty:false ~sep:" ")
  |> List.concat
  |> List.map Path.of_string_exn

exception Cycle

let equal_dep meta_path (meta_path', _, _) = Path.equal meta_path meta_path'

let sort libs =
  let rec go acc later todo progress =
    match (todo, later) with
    | [], [] -> List.rev acc
    | [], _ -> if progress then go acc [] later false else raise Cycle
    | ((_, path, descr) as x) :: r, _ ->
        let deps = dependencies_of (path, descr) in
        let deps_already_added =
          let fn dep = List.exists (equal_dep dep) acc in
          List.for_all fn deps
        in
        if deps_already_added then go (x :: acc) later r true
        else go acc (x :: later) r progress
  in
  let starts, todo =
    List.partition
      (fun (_, path, descr) -> dependencies_of (path, descr) = [])
      libs
  in
  go starts [] todo false

let ancestors ~roots ?(predicates = [ "native"; "byte" ]) meta_path =
  let rec go acc visited = function
    | [] -> Ok acc
    | meta_path :: todo when List.mem meta_path visited -> go acc visited todo
    | meta_path :: todo -> (
        Log.debug (fun m -> m "search %a" Path.pp meta_path);
        match search ~roots ~predicates meta_path with
        | Ok pkgs ->
            let requires = List.concat (List.map dependencies_of pkgs) in
            Log.debug (fun m ->
                m "search @[<hov>%a@]" Fmt.(Dump.list Path.pp) requires);
            let pkgs =
              List.map (fun (path, descr) -> (meta_path, path, descr)) pkgs
            in
            go (List.rev_append pkgs acc) (meta_path :: visited)
              (List.rev_append requires todo)
        | Error _ as err -> err)
  in
  let open Rresult in
  go [] [] [ meta_path ] >>| sort

let to_artifacts pkgs =
  let ( let* ) = Result.bind in
  let fn acc (path, pkg) =
    match acc with
    | Error _ as err -> err
    | Ok acc ->
        let directory = List.assoc_opt "directory" pkg in
        let* directory =
          match directory with
          | Some [ dir ] -> Ok Fpath.(path / dir)
          | Some _ ->
              error_msgf "Multiple directories referenced by %a" Fpath.pp
                Fpath.(path / "META")
          | None -> Ok path
        in
        let directory = Fpath.to_dir_path directory in
        let archive = List.assoc_opt "archive" pkg in
        let archive = Stdlib.Option.value ~default:[] archive in
        let plugin = List.assoc_opt "plugin" pkg in
        let plugin = Stdlib.Option.value ~default:[] plugin in
        let archive = List.map (Fpath.add_seg directory) archive in
        let plugin = List.map (Fpath.add_seg directory) plugin in
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
