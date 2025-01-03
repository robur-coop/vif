let src = Logs.Src.create "vif.top"

module Log = (val Logs.src_log src : Logs.LOG)

type cfg = { stdlib: Fpath.t; roots: Fpath.t list }

let errors = ref false

module Lexbuf = struct
  open Lexing

  let toplevel_fname = "//vif//"

  let shift_toplevel_position ~start pos =
    {
      pos_fname= toplevel_fname
    ; pos_lnum= pos.pos_lnum - start.pos_lnum + 1
    ; pos_bol= pos.pos_bol - start.pos_cnum - 1
    ; pos_cnum= pos.pos_cnum - start.pos_cnum
    }

  let shift_toplevel_location ~start loc =
    let open Location in
    {
      loc with
      loc_start= shift_toplevel_position ~start loc.loc_start
    ; loc_end= shift_toplevel_position ~start loc.loc_end
    }

  let semisemi_action =
    let lexbuf = Lexing.from_string ";;" in
    match Lexer.token lexbuf with
    | Parser.SEMISEMI -> lexbuf.Lexing.lex_last_action
    | _ -> assert false

  let map_error_loc ~fn (error : Location.error) =
    let fn_msg (msg : Location.msg) = { msg with loc= fn msg.loc } in
    { error with main= fn_msg error.main; sub= List.map fn_msg error.sub }

  let shift_location_error start =
    map_error_loc ~fn:(shift_toplevel_location ~start)

  let position_mapper start =
    let open Ast_mapper in
    let start = { start with pos_fname= toplevel_fname } in
    let location mapper loc =
      shift_toplevel_location ~start (default_mapper.location mapper loc)
    in
    { default_mapper with location }
end

module Phrase = struct
  open Lexing
  open Parsetree

  type t = {
      startpos: position
    ; parsed: (Parsetree.toplevel_phrase, exn) result
  }

  let result t = t.parsed
  let start t = t.startpos

  let error_of_exn exn =
    match Location.error_of_exn exn with
    | None -> None
    | Some `Already_displayed -> None
    | Some (`Ok error) -> Some error

  let parse lines =
    let contents = String.concat "\n" lines in
    let lexbuf = Lexing.from_string contents in
    let startpos = lexbuf.Lexing.lex_start_p in
    let parsed =
      match !Toploop.parse_toplevel_phrase lexbuf with
      | phrase -> Ok phrase
      | exception exn ->
          let exn =
            match error_of_exn exn with
            | None -> raise exn
            | Some error ->
                Location.Error (Lexbuf.shift_location_error startpos error)
          in
          begin
            if lexbuf.Lexing.lex_last_action <> Lexbuf.semisemi_action then
              let rec go () =
                match Lexer.token lexbuf with
                | Parser.SEMISEMI | Parser.EOF -> ()
                | exception Lexer.Error (_, _) -> ()
                | _ -> go ()
              in
              go ()
          end;
          Error exn
    in
    { startpos; parsed }

  let parse lines =
    match parse lines with exception End_of_file -> None | t -> Some t

  let top_directive_name (toplevel_phrase : Parsetree.toplevel_phrase) =
    match toplevel_phrase with
    | Ptop_def _ -> None
    | Ptop_dir { pdir_name= { txt; _ }; _ } -> Some txt

  let _is_findlib_directive =
    let findlib_directive = function
      | "require" | "camlp4o" | "camlp4r" | "thread" -> true
      | _ -> false
    in
    function
    | { parsed= Ok toplevel_phrase; _ } -> begin
        match top_directive_name toplevel_phrase with
        | Some dir -> findlib_directive dir
        | None -> false
      end
    | _ -> false
end

let load cfg str =
  let ( let* ) = Result.bind in
  Log.debug (fun m -> m "load: %s" str);
  let* path = Vif_meta.Path.of_string str in
  let* deps =
    Vif_meta.ancestors ~roots:cfg.roots ~predicates:[ "native" ] path
  in
  let fn acc (_, path, descr) =
    let path =
      match List.assoc_opt "directory" descr with
      | Some (dir :: _) -> Fpath.(to_dir_path (path / dir))
      | Some [] | None -> path
    in
    match List.assoc_opt "plugin" descr with
    | Some (plugin :: _) -> Fpath.(path / plugin) :: acc
    | Some [] | None -> acc
  in
  let artifacts = List.fold_left fn [] deps in
  let artifacts = List.rev artifacts in
  Log.debug (fun m -> m "load: @[<hov>%a@]" Fmt.(Dump.list Fpath.pp) artifacts);
  Ok artifacts

let load cfg str =
  match load cfg str with
  | Ok artifacts ->
      let fn artifact =
        let dir = Fpath.parent artifact in
        Topdirs.dir_directory Fpath.(to_string dir);
        Topdirs.dir_load Fmt.stderr (Fpath.to_string artifact)
      in
      List.iter fn artifacts
  | Error (`Msg msg) -> Log.err (fun m -> m "Impossible to load %S: %s" str msg)

let init cfg =
  let ppf = Fmt.stderr in
  Sys.interactive := false;
  Clflags.native_code := true;
  Clflags.debug := true;
  Topcommon.update_search_path_from_env ();
  Compenv.readenv ppf Compenv.Before_args;
  (* Clflags.add_arguments __LOC__ Option.list; *)
  (* Compenv.parse_arguments ~current argv file_argument program; *)
  (* Compmisc.read_clflags_from_env (); *)
  (* - Toploop.prepare ppf () *)
  Topcommon.set_paths ();
  Toploop.initialize_toplevel_env ();
  let objs = !Compenv.first_objfiles in
  List.iter (Topdirs.dir_load ppf) objs;
  Topcommon.run_hooks Topcommon.Startup;
  Compmisc.init_path ();
  (* Toploop.loop Format.std_formatter *)
  Topcommon.run_hooks Topcommon.After_setup;
  Toploop.add_directive "require"
    (Toploop.Directive_string (load cfg))
    { Toploop.section= "Vif loader"; doc= "Load a package" }

let config ~stdlib roots =
  let cfg = { stdlib; roots } in
  init cfg; cfg

let eval _cfg ppf ph =
  match Phrase.result ph with
  | Error exn -> raise exn
  | Ok phrase -> begin
      Warnings.reset_fatal ();
      let mapper = Lexbuf.position_mapper (Phrase.start ph) in
      let phrase =
        match phrase with
        | Parsetree.Ptop_def str ->
            Parsetree.Ptop_def (mapper.Ast_mapper.structure mapper str)
        | Ptop_dir _ as v -> v
      in
      let phrase =
        match phrase with
        | Ptop_dir _ as v -> v
        | Ptop_def str ->
            Ptop_def (Pparse.apply_rewriters_str ~tool_name:"vif" str)
      in
      if !Clflags.dump_parsetree then Printast.top_phrase ppf phrase;
      if !Clflags.dump_source then Pprintast.top_phrase ppf phrase;
      Env.reset_cache_toplevel ();
      try Toploop.execute_phrase true (* verbose *) ppf phrase
      with Compenv.Exit_with_status code ->
        Format.fprintf ppf "[%d]@." code;
        false
    end

let redirect : fn:(capture:(Buffer.t -> unit) -> 'a) -> 'a =
 fun ~fn ->
  let filename = Filename.temp_file "vif-" ".stdout" in
  Log.debug (fun m -> m "redirect stdout/stderr into %s" filename);
  let stdout' = Unix.dup ~cloexec:true Unix.stdout in
  let stderr' = Unix.dup ~cloexec:true Unix.stderr in
  let fd =
    Unix.openfile filename Unix.[ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o600
  in
  Unix.dup2 ~cloexec:false fd Unix.stdout;
  Unix.dup2 ~cloexec:false fd Unix.stderr;
  let ic = open_in filename in
  let read_up_to = ref 0 in
  let capture buf =
    flush stdout;
    flush stderr;
    let pos = Unix.lseek fd 0 Unix.SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Buffer.add_channel buf ic len
  in
  let finally () =
    close_in_noerr ic;
    Unix.close fd;
    Unix.dup2 ~cloexec:false stdout' Unix.stdout;
    Unix.dup2 ~cloexec:false stderr' Unix.stderr;
    Unix.close stdout';
    Unix.close stderr'
  in
  Fun.protect ~finally @@ fun () -> fn ~capture

type vv = V : 'a ref * 'a -> vv

let protect_vars =
  let set_vars lst = List.iter (fun (V (r, v)) -> r := v) lst in
  fun vars ~fn ->
    let backup = List.map (fun (V (r, _)) -> V (r, !r)) vars in
    set_vars vars;
    let finally () = set_vars backup in
    Fun.protect ~finally fn

let capture_compiler_stuff ppf fn =
  protect_vars [ V (Location.formatter_for_warnings, ppf) ] ~fn

let trim str =
  let len = String.length str in
  if len = 0 then str
  else
    let trim_from = if str.[0] = '\n' then 1 else 0 in
    let trim_to = if str.[len - 1] = '\n' then len - 1 else len in
    if trim_to - trim_from <= 0 then ""
    else String.sub str trim_from (trim_to - trim_from)

let rec ltrim = function "" :: r -> ltrim r | lst -> lst
let rtrim lst = List.rev (ltrim (List.rev lst))
let trim lst = ltrim (rtrim (List.map trim lst))

let rec ends_by_semi_semi = function
  | [] -> false
  | [ x ] ->
      String.length x >= 2
      && x.[String.length x - 1] = ';'
      && x.[String.length x - 2] = ';'
  | _ :: r -> ends_by_semi_semi r

let cut_into_phrases lst =
  let rec go acc phrase = function
    | [] -> List.rev (List.rev phrase :: acc)
    | x :: r when ends_by_semi_semi [ x ] ->
        go (List.rev (x :: phrase) :: acc) [] r
    | x :: r -> go acc (x :: phrase) r
  in
  go [] [] lst

let eval cfg cmd =
  let buf = Buffer.create 0x7ff in
  let ppf = Format.formatter_of_out_channel stderr in
  errors := false;
  let eval ~capture phrase =
    let lines = ref [] in
    let capture () =
      capture buf;
      match Buffer.contents buf with
      | "" -> ()
      | str ->
          Buffer.clear buf;
          lines := str :: !lines
    in
    let out_phrase = !Oprint.out_phrase in
    let fn_out_phrase ppf = function
      | Outcometree.Ophr_exception _ as phr -> out_phrase ppf phr
      | phr -> capture (); out_phrase ppf phr; capture ()
    in
    Oprint.out_phrase := fn_out_phrase;
    let restore () = Oprint.out_phrase := out_phrase in
    begin
      match eval cfg ppf phrase with
      | ok ->
          errors := (not ok) || !errors;
          restore ()
      | exception exn ->
          errors := true;
          restore ();
          Location.report_exception ppf exn
    end;
    Format.pp_print_flush ppf ();
    capture ();
    trim (List.rev !lines)
  in
  Log.debug (fun m -> m "Start to eval: %a" Fmt.(Dump.list (fmt "%S")) cmd);
  let fn ~capture =
    capture_compiler_stuff ppf @@ fun () ->
    let cmd =
      match cmd with [] | [ _ ] -> cmd | x :: r -> x :: List.map (( ^ ) " ") r
    in
    let phrases = cut_into_phrases cmd in
    let phrases =
      List.map
        (fun phrase ->
          match Phrase.parse phrase with
          | Some t -> eval ~capture t
          | None -> [])
        phrases
    in
    let phrases = List.concat phrases in
    if !errors then Error phrases else Ok phrases
  in
  redirect ~fn
