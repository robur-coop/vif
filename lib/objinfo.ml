let src = Logs.Src.create "uniq.info"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

[@@@warning "-32"]

let ( % ) f g x = f (g x)

type t = {
    name: Unitname.t
  ; version: int option
  ; exports: (Modname.t * Digest.t option) list
  ; intfs: elt list
  ; impls: elt list
  ; format: format
}

and elt =
  | Qualified of Modname.t * Digest.t
  | Fully_qualified of Modname.t * Digest.t * Fpath.t
  | Located of Modname.t * Fpath.t
  | Named of Modname.t

and 'a kind =
  | Cmo : Cmo_format.compilation_unit kind
  | Cma : Cmo_format.library kind
  | Cmi : Cmi_format.cmi_infos kind
  | Cmx : Cmx_format.unit_infos kind
  | Cmxa : Cmx_format.library_infos kind

and format = Format : 'a kind * 'a -> format

let equal a b =
  String.equal (Unitname.filepath a.name) (Unitname.filepath b.name)

exception Inconsistency of Unitname.t * Modname.t * Digest.t * Digest.t

let inconsistency location name crc crc' =
  let unit = Unitname.modulize (Fpath.to_string location) in
  raise (Inconsistency (unit, name, crc, crc'))

let is_fully_resolved t =
  List.for_all (function Fully_qualified _ -> true | _ -> false) t.intfs
  && List.for_all (function Fully_qualified _ -> true | _ -> false) t.impls

let is_a_library t =
  match t.format with
  | Format (Cma, _) -> true
  | Format (Cmxa, _) -> true
  | _ -> false

let is_native t =
  match t.format with
  | Format (Cmx, _) -> true
  | Format (Cmxa, _) -> true
  | Format (Cmi, _) -> true
  | _ -> false

let is_an_interface t =
  match t.format with Format (Cmi, _) -> true | _ -> false

let is_a_cmi t = match t.format with Format (Cmi, _) -> true | _ -> false

let of_elt = function
  | Qualified (m, crc) -> (m, Some crc)
  | Fully_qualified (m, crc, _) -> (m, Some crc)
  | Located (m, _) -> (m, None)
  | Named m -> (m, None)

let exports t = t.exports
let location t = Fpath.v (Unitname.filepath t.name)
let intfs_imported t = List.map of_elt t.intfs
let impls_imported t = List.map of_elt t.impls
let modname t = Unitname.modname t.name

let missing t =
  let intfs, _ =
    List.partition_map
      (function
        | Named m | Qualified (m, _) -> Either.Left m | _ -> Either.Right ())
      t.intfs
  in
  let impls, _ =
    List.partition_map
      (function
        | Named m | Qualified (m, _) -> Either.Left m | _ -> Either.Right ())
      t.impls
  in
  (intfs, impls)

let crc_of t m =
  let ( >>| ) x f = Stdlib.Option.map f x in
  List.find_opt (fun (m', _) -> Modname.compare m m' = 0) t.exports
  >>| snd
  |> Option.join

let kind t =
  match t.format with
  | Format (Cmo, _) | Format (Cma, _) | Format (Cmx, _) | Format (Cmxa, _) ->
      `Impl
  | Format (Cmi, _) -> `Intf

let elt_name = function
  | Qualified (x, _) | Fully_qualified (x, _, _) | Named x | Located (x, _) -> x

let elt_replace elt elts =
  List.fold_left
    (fun acc elt' ->
      let a = elt_name elt in
      let b = elt_name elt' in
      if Modname.compare a b = 0 then elt :: acc else elt' :: acc)
    [] elts
  |> List.rev

let qualify t ?location ?crc kind modname =
  let elt =
    match (location, crc) with
    | None, Some crc -> Qualified (modname, crc)
    | Some location, Some crc -> Fully_qualified (modname, crc, location)
    | None, None -> Named modname
    | Some location, None -> Located (modname, location)
  in
  match kind with
  | `Intf -> { t with intfs= elt_replace elt t.intfs }
  | `Impl -> { t with impls= elt_replace elt t.impls }

let elt_compare a b =
  let a = elt_name a in
  let b = elt_name b in
  Modname.compare a b

let elt_find modname lst =
  try
    List.find
      (fun elt ->
        let modname' = elt_name elt in
        Modname.compare modname modname' = 0)
      lst
    |> function
    | Qualified (_, crc) -> [ (modname, Some crc) ]
    | Fully_qualified (_, crc, _) -> [ (modname, Some crc) ]
    | Named _ -> [ (modname, None) ]
    | Located _ -> [ (modname, None) ]
  with Not_found -> []

open Bos
open Rresult

let to_elt (str, crc) =
  match crc with
  | Some crc -> Qualified (Modname.v str, crc)
  | None -> Named (Modname.v str)

let info_of_cmi ~location ~version _ic =
  match Cmt_format.read (Fpath.to_string location) with
  | None, _ -> error_msgf "Invalid cmi object: %a" Fpath.pp location
  | Some cmi, _ ->
      let intfs = cmi.Cmi_format.cmi_crcs in
      let intfs = List.map to_elt intfs in
      let intfs = List.sort elt_compare intfs in
      let impls = [] in
      let format = Format (Cmi, cmi) in
      let name = Unitname.modulize (Fpath.to_string location) in
      let exports = elt_find (Unitname.modname name) intfs in
      Ok { name; version; exports; intfs; impls; format }
  | exception _ -> error_msgf "Invalid cmi object: %a" Fpath.pp location

let info_of_cmo ~location ~version ic =
  let cu_pos = input_binary_int ic in
  seek_in ic cu_pos;
  let cu = (input_value ic : Cmo_format.compilation_unit) in
  let intfs = List.map to_elt cu.cu_imports in
  let intfs = List.sort elt_compare intfs in
  let impls = [] in
  let format = Format (Cmo, cu) in
  let name = Unitname.modulize (Fpath.to_string location) in
  let exports = [ (Unitname.modname name, None) ] in
  Ok { name; version; exports; intfs; impls; format }

let info_of_cmx ~location ~version ic =
  let ui = (input_value ic : Cmx_format.unit_infos) in
  let name = Unitname.modulize (Fpath.to_string location) in
  let exports = [ (Unitname.modname name, Some (Digest.input ic)) ] in
  let intfs = List.map to_elt ui.ui_imports_cmi in
  let intfs = List.sort elt_compare intfs in
  let impls = List.map to_elt ui.ui_imports_cmx in
  let impls = List.sort elt_compare impls in
  let format = Format (Cmx, ui) in
  Ok { name; version; exports; intfs; impls; format }

let to_elt (modname, crc) =
  match crc with Some crc -> Qualified (modname, crc) | None -> Named modname

let info_of_cma ~location ~version ic =
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let toc = (input_value ic : Cmo_format.library) in
  let importss =
    List.map (fun { Cmo_format.cu_imports; _ } -> cu_imports) toc.lib_units
  in
  let fold m (str, crc) =
    let name = Modname.v str in
    match (Modname.Map.find_opt name m, crc) with
    | None, _ -> Modname.Map.add name crc m
    | Some None, _ | Some (Some _), None ->
        Modname.Map.add name crc (Modname.Map.remove name m)
    | Some (Some crc'), Some crc ->
        if crc <> crc' then inconsistency location name crc crc';
        m
  in
  let imports = List.concat importss in
  let m = List.fold_left fold Modname.Map.empty imports in
  let exports =
    List.map
      (fun { Cmo_format.cu_name= Compunit cu_name; _ } ->
        (Modname.v cu_name, None))
      toc.lib_units
  in
  let intfs = Modname.Map.bindings m in
  let intfs = List.map to_elt intfs in
  let impls = [] in
  let format = Format (Cma, toc) in
  let name = Unitname.modulize (Fpath.to_string location) in
  Ok { name; version; exports; intfs; impls; format }

let info_of_cmxa ~location ~version ic =
  let li = (input_value ic : Cmx_format.library_infos) in
  let importss_cmi =
    List.map
      (fun ({ Cmx_format.ui_imports_cmi; _ }, _crc) -> ui_imports_cmi)
      li.lib_units
  in
  let importss_cmx =
    List.map
      (fun ({ Cmx_format.ui_imports_cmx; _ }, _crc) -> ui_imports_cmx)
      li.lib_units
  in
  let fold m (str, crc) =
    let name = Modname.v str in
    match (Modname.Map.find_opt name m, crc) with
    | None, _ -> Modname.Map.add name crc m
    | Some None, _ | Some (Some _), None ->
        Modname.Map.add name crc (Modname.Map.remove name m)
    | Some (Some crc'), Some crc ->
        if crc <> crc' then inconsistency location name crc crc';
        m
  in
  let exports =
    List.map
      (fun ({ Cmx_format.ui_name; _ }, crc) -> (Modname.v ui_name, Some crc))
      li.lib_units
  in
  let m = List.fold_left fold Modname.Map.empty (List.concat importss_cmi) in
  let intfs = Modname.Map.bindings m in
  let intfs = List.map to_elt intfs in
  let m = List.fold_left fold Modname.Map.empty (List.concat importss_cmx) in
  let impls = Modname.Map.bindings m in
  let impls = List.map to_elt impls in
  let format = Format (Cmxa, li) in
  let name = Unitname.modulize (Fpath.to_string location) in
  Ok { name; version; exports; intfs; impls; format }

let is_intf location = Fpath.mem_ext [ ".mli" ] location

let from_object location { Misc.Magic_number.kind; version } ic =
  let version = Some version in
  match kind with
  | Misc.Magic_number.Cmi -> info_of_cmi ~location ~version ic
  | Cmo -> info_of_cmo ~location ~version ic
  | Cma -> info_of_cma ~location ~version ic
  | Cmx _ -> info_of_cmx ~location ~version ic
  | Cmxa _ -> info_of_cmxa ~location ~version ic
  | _ -> error_msgf "Unexpected OCaml object: %a" Fpath.pp location

let v location =
  OS.File.with_ic location @@ fun ic () ->
  match Misc.Magic_number.read_info ic with
  | Ok info -> from_object location info ic
  | Error _ -> error_msgf "Invalid object: %a" Fpath.pp location

let pp ppf t = Fmt.string ppf (Unitname.filepath t.name)

let v location =
  match v location () |> R.join with
  | value -> value
  | exception Inconsistency (unit, name, crc, crc') ->
      error_msgf
        "Inconsistency between interfaces:\n\
         The given library %s requires two times the interface %a with\n\
         1) the digest %a\n\
         2) and the digest %a\n"
        (Unitname.filepath unit) Modname.pp name Digest.pp crc Digest.pp crc'

let vs lst =
  let ( let* ) = Result.bind in
  let fn acc path =
    match acc with
    | Error _ as err -> err
    | Ok acc ->
        let* a = v path in
        Ok (a :: acc)
  in
  List.fold_left fn (Ok []) lst

let dummy = String.make Digest.length '-'

let show_elt ppf = function
  | Qualified (name, crc) ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold Digest.pp)
        crc
        Fmt.(styled `Yellow Modname.pp)
        name
  | Fully_qualified (name, crc, location) ->
      Fmt.pf ppf "\t%a\t%a (%a)\n%!"
        Fmt.(styled `Bold Digest.pp)
        crc
        Fmt.(styled `Yellow Modname.pp)
        name
        Fmt.(styled `Green Fpath.pp)
        location
  | Named name ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold string)
        dummy
        Fmt.(styled `Yellow Modname.pp)
        name
  | Located (name, location) ->
      Fmt.pf ppf "\t%a\t%a (%a)\n%!"
        Fmt.(styled `Bold string)
        dummy
        Fmt.(styled `Yellow Modname.pp)
        name
        Fmt.(styled `Green Fpath.pp)
        location

let show_export ppf (name, crc) =
  match crc with
  | None ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold string)
        dummy
        Fmt.(styled `Yellow Modname.pp)
        name
  | Some crc ->
      Fmt.pf ppf "\t%a\t%a\n%!"
        Fmt.(styled `Bold Digest.pp)
        crc
        Fmt.(styled `Yellow Modname.pp)
        name

let show ppf t =
  Fmt.pf ppf "File: %a\n%!"
    Fmt.(styled `Green string)
    (Unitname.filepath t.name);
  Fmt.pf ppf "Name: %a\n%!"
    Fmt.(styled `Yellow Modname.pp)
    (Unitname.modname t.name);
  if Stdlib.Option.is_some t.version then
    Fmt.pf ppf "Version: %d\n%!" (Stdlib.Option.get t.version);
  if t.intfs <> [] then Fmt.pf ppf "Interfaces imported:\n%!";
  List.iter (Fmt.pf ppf "%a" show_elt) t.intfs;
  if t.impls <> [] then Fmt.pf ppf "Implementations imported:\n%!";
  List.iter (Fmt.pf ppf "%a" show_elt) t.impls;
  Fmt.pf ppf "Export:\n%!";
  List.iter (Fmt.pf ppf "%a" show_export) t.exports
