let src = Logs.Src.create "vif.r"

module Log = (val Logs.src_log src : Logs.LOG)

module Ext = struct
  let slash = Re.char '/'
  let comma = Re.char ','
  let amper = Re.char '&'

  (** -?[0-9]+( .[0-9]* )? *)
  let float =
    let open Re in
    seq [ opt (char '-'); rep1 digit; opt (seq [ char '.'; rep digit ]) ]

  (** -?[0-9]+ *)
  let arbitrary_int =
    let open Re in
    seq [ opt (char '-'); rep1 digit ]

  (** true|false *)
  let bool =
    let open Re in
    alt [ str "true"; str "false" ]

  (** Non empty list of safe chars *)
  let string component =
    let open Re in
    match component with
    | `Path -> rep1 @@ compl [ slash ]
    | `Query_value -> rep1 @@ compl [ set "&;+," ]

  (** Separated by , or by / *)
  let list ?m ~component n re =
    let open Re in
    match component with
    | `Path -> repn (seq [ slash; re ]) n m
    | `Query_value ->
        if n = 0 then alt [ epsilon; seq [ re; repn (seq [ comma; re ]) 0 m ] ]
        else seq [ re; repn (seq [ comma; re ]) (n - 1) m ]

  let query_sep ~any =
    if not any then amper
    else
      let open Re in
      seq [ amper; rep @@ seq [ rep1 @@ compl [ amper ]; amper ] ]
end

module Utils = struct
  let map_snd f (x, y) = (x, f y)

  let rec intersperse sep = function
    | [] -> []
    | [ x ] -> [ x ]
    | h :: t -> h :: sep :: intersperse sep t

  (** Offset of [el] in [l], given the function count. Used to get the first
      regexp group at a given place. *)
  let find_idx count el l =
    let rec aux el i = function
      | [] -> raise Not_found
      | x :: l' -> if x == el then i else aux el (i + count el) l'
    in
    aux el 0 l

  (* Invariants:
     - [l_before] is included in [l_after].
     - No duplicates (see note on {!find_idx}).
  *)

  (** if [l' ∈ l] then [build_permutation offset count l l'] builds a mapping:
      index in [l => offset in l']. Offsets are computed respecting [offset] and
      [count]. *)
  let build_permutation offset count l_before l_after =
    let t = Array.make (List.length l_before) 0 in
    l_before
    |> List.iteri (fun i x ->
           let j = find_idx count x l_after in
           t.(i) <- offset + j);
    t
end

open Tyre.Internal

let sort_query l = List.sort (fun (x, _) (y, _) -> compare (x : string) y) l

type 'a re_atom = 'a Tyre.Internal.wit

(** Top level atoms are specialized for path and query, see documentation. *)
let re_atom re = Tyre.Internal.build re

let re_atom_path : type a. int -> a raw -> int * a re_atom * Re.t list =
  let open Re in
  fun i -> function
    | Rep e ->
        let _, w, re = re_atom 1 e in
        ( i + 1
        , Rep (i, w, Re.compile re)
        , [ group @@ Ext.list ~component:`Path 0 @@ no_group re ] )
    | Opt e ->
        let i', w, re = re_atom i e in
        let id, re = mark re in
        (i', Opt (id, w), [ alt [ epsilon; seq [ Ext.slash; re ] ] ])
    | e ->
        let i', w, re = re_atom i e in
        (i', w, [ Ext.slash; re ])

let re_atom_query : type a. int -> a raw -> int * a re_atom * Re.t =
  let open Re in
  fun i -> function
    | Rep e ->
        let _, w, re = re_atom 1 e in
        ( i + 1
        , Rep (i, w, Re.compile re)
        , group @@ Ext.list ~component:`Query_value 0 @@ no_group re )
    | e ->
        let i', w, re = re_atom i e in
        (i', w, re)

type (_, _) re_path =
  | Start : ('r, 'r) re_path
  | PathAtom : ('f, 'a -> 'r) re_path * 'a re_atom -> ('f, 'r) re_path

let rec re_path : type r f.
    int -> (f, r) Vif_u.path -> int * (f, r) re_path * Re.t list =
  let open Re in
  fun i -> function
    | Host s ->
        let re = Re.str @@ Uri.pct_encode ~component:`Host s in
        (i, Start, [ re ])
    | Rel -> (i, Start, [])
    | Path_const (p, s) ->
        let i', p, re = re_path i p in
        (i', p, str s :: Ext.slash :: re)
    | Path_atom (p, a) ->
        let i', wp, rp = re_path i p in
        let i'', wa, ra = re_atom_path i' @@ from_t a in
        (i'', PathAtom (wp, wa), List.rev_append ra rp)

type ('fu, 'ret) re_query =
  | Nil : ('r, 'r) re_query
  | Any : ('r, 'r) re_query
  | Cons : 'a re_atom * ('f, 'r) re_query -> ('a -> 'f, 'r) re_query

let rec collect_re_query : type r f.
       (f, r) Vif_u.query
    -> int * (f, r) re_query * bool * (string * (Re.t * int)) list = function
  | Nil -> (0, Nil, false, [])
  | Any -> (0, Any, true, [])
  | Query_atom (s, a, q) ->
      let grps, wa, ra = re_atom_query 0 @@ from_t a in
      let total_grps, wq, b_any, rq = collect_re_query q in
      let total_grps = total_grps + grps in
      (total_grps, Cons (wa, wq), b_any, (s, (ra, grps)) :: rq)

let rec shift_lits : type a. int -> a re_atom -> a re_atom =
 fun shift -> function
  | Lit i -> Lit (i + shift)
  | Conv (x, f) -> Conv (shift_lits shift x, f)
  | Opt (m, x) -> Opt (m, shift_lits shift x)
  | Alt (m, x1, x2) -> Alt (m, shift_lits shift x1, shift_lits shift x2)
  | Seq (x1, x2) -> Seq (shift_lits shift x1, shift_lits shift x2)
  | Rep (i, x, r) -> Rep (shift + i, x, r)

let rec permut_query : type r f.
    int -> int array -> (r, f) re_query -> (r, f) re_query =
 fun n permutation -> function
  | Nil -> Nil
  | Any -> Any
  | Cons (wa, wq) ->
      let shift = permutation.(n) in
      let wa = shift_lits shift wa in
      Cons (wa, permut_query (n + 1) permutation wq)

let re_query current_idx q =
  let grps, wq, b, rql = collect_re_query q in
  let rel = sort_query rql in
  let p = Utils.build_permutation current_idx (fun (_, (_, i)) -> i) rql rel in
  let wq = permut_query 0 p wq in
  (grps, wq, b, rel)

type ('f, 'r) re_url =
  | ReUrl : ('f, 'x) re_path * ('x, 'r) re_query -> ('f, 'r) re_url

let re_url : type f r. int -> (f, r) Vif_u.t -> int * (f, r) re_url * Re.t =
 fun i -> function
  | Url (slash, p, q) -> (
      let end_path =
        match slash with
        | No_slash -> Re.epsilon
        | Slash -> Re.char '/'
        | Maybe_slash -> Re.(opt @@ char '/')
      in
      let idx, wp, rp = re_path i p in
      match q with
      | Nil -> (idx, ReUrl (wp, Nil), Re.seq @@ List.rev (end_path :: rp))
      | Any ->
          let end_re = Re.(opt @@ seq [ Re.char '?'; rep any ]) in
          ( idx
          , ReUrl (wp, Nil)
          , Re.seq @@ List.rev_append rp [ end_path; end_re ] )
      | _ ->
          let grps, wq, any_query, rel = re_query idx q in
          let query_sep = Ext.query_sep ~any:any_query in
          let add_around_query =
            if not any_query then fun x -> x else fun l -> Re.(rep any) :: l
          in
          let fn l (s, (re, _)) = Re.seq [ Re.str (s ^ "="); re ] :: l in
          let re =
            rel
            |> List.fold_left fn []
            |> Utils.intersperse query_sep
            |> add_around_query
            |> List.rev
            |> add_around_query
          in
          let re =
            Re.seq @@ List.rev_append rp (end_path :: Re.char '?' :: re)
          in
          (idx + grps, ReUrl (wp, wq), re))

let get_re url =
  let _, _, re = re_url 1 url in
  re

(** {3 Extraction.} *)

(** Extracting atom is just a matter of following the witness. We just need to
    take care of counting where we are in the matching groups. *)
let extract_atom = extract

(** Since path is in reversed order, we proceed by continuation. *)
let rec extract_path : type f x r.
    original:string -> (f, x) re_path -> Re.Group.t -> (x -> r) -> f -> r =
 fun ~original wp subs k ->
  match wp with
  | Start -> k
  | PathAtom (rep, rea) ->
      let v = extract_atom ~original rea subs in
      let k f = k (f v) in
      extract_path ~original rep subs k

(** Query are in the right order, we can proceed in direct style. *)
let rec extract_query : type x r.
    original:string -> (x, r) re_query -> Re.Group.t -> x -> r =
 fun ~original wq subs f ->
  match wq with
  | Nil -> f
  | Any -> f
  | Cons (rea, req) ->
      let v = extract_atom ~original rea subs in
      extract_query ~original req subs (f v)

let extract_url : type r f.
    original:string -> (f, r) re_url -> Re.Group.t -> f -> r =
 fun ~original (ReUrl (wp, wq)) subs f ->
  let k = extract_query ~original wq subs in
  let k = extract_path ~original wp subs k in
  k f

let prepare_uri uri =
  uri |> Uri.query |> sort_query |> Uri.with_query uri |> Uri.path_and_query

let extract url =
  let _idx, re_url, re = re_url 1 url in
  let re = Re.(compile @@ whole_string re) in
  fun ~f uri ->
    let s = prepare_uri uri in
    let subs = Re.exec re s in
    extract_url ~original:s re_url subs f

(** {4 Multiple match} *)

type ('fu, 'return) req =
  | Request :
      Vif_method.t option * ('c, 'a) Vif_t.t
      -> (('c, 'a) Vif_request.t -> 'r, 'r) req

type 'r route = Route : ('f, 'x) req * ('x, 'r) Vif_u.t * 'f -> 'r route

let route req t f = Route (req, t, f)

type 'r re_ex =
  | ReEx : ('f, 'x) req * 'f * Re.Mark.t * ('x, 'r) re_url -> 'r re_ex

(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_info_list idx = function
  | [] -> ([], [])
  | Route (req, url, f) :: l ->
      let idx, re_url, re = re_url idx url in
      let rel, wl = build_info_list idx l in
      let id, re = Re.mark re in
      (re :: rel, ReEx (req, f, id, re_url) :: wl)

(*
type 'a atom = 'a Tyre.Internal.wit

let atom re = Tyre.Internal.build re
let slash = Re.char '/'
let comma = Re.char ','
let amper = Re.char '&'

let list ?m ~component n re =
  let open Re in
  match component with
  | `Path -> repn (seq [ slash; re ]) n m
  | `Query_value ->
      if n = 0 then alt [ epsilon; seq [ re; repn (seq [ comma; re ]) 0 m ] ]
      else seq [ re; repn (seq [ comma; re ]) (n - 1) m ]

let atom_path : type a. int -> a Tyre.Internal.raw -> int * a atom * Re.t list =
  let open Re in
  fun i -> function
    | Rep e ->
        let _, w, re = atom 1 e in
        ( i + 1
        , Rep (i, w, Re.compile re)
        , [ group (list ~component:`Path 0 (no_group re)) ] )
    | Opt e ->
        let i', w, re = atom i e in
        let id, re = mark re in
        (i', Opt (id, w), [ alt [ epsilon; seq [ slash; re ] ] ])
    | e ->
        let i', w, re = atom i e in
        (i', w, [ slash; re ])

let atom_query : type a. int -> a Tyre.Internal.raw -> int * a atom * Re.t =
  let open Re in
  fun i -> function
    | Rep e ->
        let _, w, re = atom 1 e in
        Log.debug (fun m -> m "%02d -> %a" (i + 1) Tyre.pp (Tyre.Internal.to_t e));
        ( i + 1
        , Rep (i, w, Re.compile re)
        , (group @@ list ~component:`Query_value 0 @@ no_group re) )
    | e ->
        let i', w, re = atom i e in
        Log.debug (fun m -> m "%02d -> %a" i' Tyre.pp (Tyre.Internal.to_t e));
        (i', w, re)

type ('fu, 'return) path =
  | Start : ('r, 'r) path
  | Path_atom : ('f, 'a -> 'r) path * 'a atom -> ('f, 'r) path

let rec path : type r f.
    int -> (f, r) Vif_u.path -> int * (f, r) path * Re.t list =
 fun i -> function
  | Vif_u.Host str ->
      let re = Re.str (Pct.encode_host str) in
      (i, Start, [ re ])
  | Rel -> (i, Start, [])
  | Path_const (p, str) ->
      let i', p, re = path i p in
      (i', p, Re.str str :: slash :: re)
  | Path_atom (p, a) ->
      let i', wp, rp = path i p in
      let i'', wa, ra = atom_path i' (Tyre.Internal.from_t a) in
      (i'', Path_atom (wp, wa), List.rev_append ra rp)

type ('fu, 'return) query =
  | Nil : ('r, 'r) query
  | Any : ('r, 'r) query
  | Cons : 'a atom * ('f, 'r) query -> ('a -> 'f, 'r) query

let rec collect_query : type r f.
       (f, r) Vif_u.query
    -> int * (f, r) query * bool * (string * (Re.t * int)) list = function
  | Nil -> (0, Nil, false, [])
  | Any -> (0, Any, true, [])
  | Query_atom (s, a, q) ->
      let grps, wa, ra = atom_query 0 @@ Tyre.Internal.from_t a in
      let total_grps, wq, b_any, rq = collect_query q in
      let total_grps = total_grps + grps in
      (total_grps, Cons (wa, wq), b_any, (s, (ra, grps)) :: rq)

let rec shift_literals : type a. int -> a atom -> a atom =
 fun shift -> function
  | Tyre.Internal.Lit i -> Lit (i + shift)
  | Conv (x, fn) -> Conv (shift_literals shift x, fn)
  | Opt (m, x) -> Opt (m, shift_literals shift x)
  | Alt (m, x1, x2) -> Alt (m, shift_literals shift x1, shift_literals shift x2)
  | Seq (x1, x2) -> Seq (shift_literals shift x1, shift_literals shift x2)
  | Rep (i, x, r) -> Rep (shift + i, x, r)

let rec permut_query : type r f.
    int -> int array -> (r, f) query -> (r, f) query =
 fun n permutation -> function
  | Nil -> Nil
  | Any -> Any
  | Cons (wa, wq) ->
      let shift = permutation.(n) in
      Log.debug (fun m -> m "shift %02d -> %02d" n shift);
      let wa = shift_literals shift wa in
      Cons (wa, permut_query (n + 1) permutation wq)

let find_idx count el l =
  let rec go el i = function
    | [] -> raise Not_found
    | x :: r -> if x == el then i else go el (i + count el) r
  in
  go el 0 l

let build_permutation offset count l_before l_after =
  let t = Array.make (List.length l_before) 0 in
  let fn i x =
    let j = find_idx count x l_after in
    t.(i) <- offset + j
  in
  List.iteri fn l_before; t

let sort_query = List.sort (fun (x, _) (y, _) -> compare (x:string) y)

let query current_idx q =
  let grps, wq, b, rql = collect_query q in
  let rel = sort_query rql in
  let p = build_permutation current_idx (fun (_, (_, i)) -> i) rql rel in
  Log.debug (fun m -> m "permutation: %a" Fmt.(Dump.array int) p);
  let wq = permut_query 0 p wq in
  (grps, wq, b, rel)

type ('fu, 'return) t = Url : ('f, 'x) path * ('x, 'r) query -> ('f, 'r) t

let query_sep ~any =
  if not any then amper
  else Re.(seq [ amper; rep (seq [ rep1 (compl [ amper ]); amper ]) ])

let rec intersperse sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: r -> x :: sep :: intersperse sep r

let url : type f r. int -> (f, r) Vif_u.t -> int * (f, r) t * Re.t =
 fun i (Vif_u.Url (slash, p, q)) ->
  let end_path =
    match slash with
    | Vif_u.No_slash -> Re.epsilon
    | Slash -> Re.char '/'
    | Maybe_slash -> Re.(opt (Re.char '/'))
  in
  let idx, wp, rp = path i p in
  match q with
  | Nil -> (idx, Url (wp, Nil), Re.seq (List.rev (end_path :: rp)))
  | Any ->
      let end_re = Re.(opt (seq [ Re.char '?'; rep any ])) in
      (idx, Url (wp, Nil), Re.seq (List.rev_append rp [ end_path; end_re ]))
  | _ ->
      let grps, wq, any_query, rel = query idx q in
      let query_sep = query_sep ~any:any_query in
      let add_around_query =
        if not any_query then Fun.id else fun l -> Re.(rep any) :: l
      in
      let fn l (s, (re, _)) = Re.seq [ Re.str (s ^ "="); re ] :: l in
      let re =
        rel
        |> List.fold_left fn []
        |> intersperse query_sep
        |> add_around_query
        |> List.rev
        |> add_around_query
      in
      let re = Re.seq (List.rev_append rp (end_path :: Re.char '?' :: re)) in
      Log.debug (fun m -> m "idx:%d" (idx+ grps));
      Log.debug (fun m -> m "%a" Re.pp re);
      (idx + grps, Url (wp, wq), re)

let re t =
  let _, _, re = url 1 t in
  re

let extract = Tyre.Internal.extract

let rec extract_path : type f x r.
    original:string -> (f, x) path -> Re.Group.t -> (x -> r) -> f -> r =
 fun ~original wp subs k ->
  match wp with
  | Start -> k
  | Path_atom (rep, rea) ->
      let v = extract ~original rea subs in
      let k f = k (f v) in
      extract_path ~original rep subs k

let rec extract_query : type f r.
    original:string -> (f, r) query -> Re.Group.t -> f -> r =
 fun ~original wq subs k ->
  match wq with
  | Nil -> k
  | Any -> k
  | Cons (rea, req) ->
      let v = extract ~original rea subs in
      extract_query ~original req subs (k v)

let extract : type r f. original:string -> (f, r) t -> Re.Group.t -> f -> r =
 fun ~original (Url (wp, wq)) subs f ->
  let k = extract_query ~original wq subs in
  let k = extract_path ~original wp subs k in
  k f

(*
let extract t =
  let url, re = url t in
  let re = Re.(compile (whole_string re)) in
  fun ~f target ->
    let subs = Re.exec re target in
    extract ~original:target url subs f
*)

type ('fu, 'return) req =
  | Request :
      Vif_method.t option * ('c, 'a) Vif_t.t
      -> (('c, 'a) Vif_request.t -> 'r, 'r) req

type 'r route = Route : ('f, 'x) req * ('x, 'r) Vif_u.t * 'f -> 'r route

let route req t f = Route (req, t, f)

type 'r re = Re : ('f, 'x) req * 'f * Re.Mark.t * ('x, 'r) t -> 'r re

let rec build_info_list idx = function
  | [] -> ([], [])
  | Route (req, t, fn) :: l ->
      let idx, ret, re = url idx t in
      let rel, wl = build_info_list idx l in
      let id, re = Re.mark re in
      (re :: rel, Re (req, fn, id, ret) :: wl)
*)

type request = {
    extract:
      'c 'a.
      Vif_method.t option -> ('c, 'a) Vif_t.t -> ('c, 'a) Vif_request.t option
}

let prepare_uri uri =
  uri |> Uri.query |> sort_query |> Uri.with_query uri |> Uri.path_and_query

let rec find_and_trigger : type r.
    original:string -> request -> Re.Group.t -> r re_ex list -> r =
 fun ~original e subs -> function
  | [] ->
      (* Invariant: At least one of the regexp of the alternative matches. *)
      assert false
  | ReEx (Request (meth, c), f, id, re_url) :: l ->
      if Re.Mark.test subs id then
        match e.extract meth c with
        | None -> find_and_trigger ~original e subs l
        | Some v -> extract_url ~original re_url subs (f v)
      else find_and_trigger ~original e subs l

let dispatch : type r c.
       default:((c, string) Vif_request.t -> string -> r)
    -> r route list
    -> request:request
    -> target:string
    -> r =
 fun ~default l ->
  let rel, wl = build_info_list 1 l in
  let re = Re.(compile @@ whole_string @@ alt rel) in
  fun ~request:e ~target ->
    let s = prepare_uri (Uri.of_string target) in
    match Re.exec_opt re s with
    | None -> default (Option.get (e.extract None Any)) s
    | Some subs -> (
        try find_and_trigger ~original:s e subs wl
        with Not_found -> assert false)

(*
let dispatch : type r c.
       default:((c, string) Vif_request.t -> string -> r)
    -> r route list
    -> request:request
    -> target:string
    -> r =
 fun ~default l ->
  let rel, wl = build_info_list 1 l in
  let rel = List.map (fun re -> Re.(compile (whole_string re))) rel in
  let routes = List.combine rel wl in
  fun ~request:e ~target ->
    let rec go = function
      | [] ->
          Log.debug (fun m -> m "fallback to the default handler");
          let req = e.extract None Any in
          let req = Option.get req in
          default req target
      | (re, ReEx (Request (meth, c), fn, id, ret)) :: rest -> begin
          match Re.exec_opt re target with
          | Some subs when Re.Mark.test subs id ->
              Log.debug (fun m ->
                  m "%S matches with %a %b" target Re.Group.pp subs
                    (Re.Mark.test subs id));
              let r = if Re.Mark.test subs id then e.extract meth c else None in
              if r = None then go rest
              else extract ~original:target ret subs (fn (Option.get r))
          | _ -> go rest
        end
    in
    try go routes
    with Not_found ->
      let bt = Printexc.get_raw_backtrace () in
      Log.debug (fun m -> m "Not_found exception raised by:");
      Log.debug (fun m -> m "%s" (Printexc.raw_backtrace_to_string bt));
      let req = e.extract None Any in
      let req = Option.get req in
      default req target
*)

(* TODO(dinosaure): A real method would be to "merge" the routes that are
   equivalent (structurally and by type) but this would involve implementing an
   equal function ([Refl]) in [Vif_u] which would be... laborious. We are
   therefore satisfied with testing several regexes instead of compiling only
   one (with [Re.alt]) as was the case initially.

   The case we are trying to solve and the possibility of defining the same
   route but managing different content (managing a form or JSON). *)

(*
    match Re.exec_opt re target with
    | None ->
        Log.debug (fun m -> m "Fallback to the default route");
        let[@warning "-8"] (Some request) = request.extract None Any in
        default request target
    | Some subs -> begin
        try find_and_trigger ~original:target ~uid:0 ~request subs wl
        with Not_found as exn ->
          Log.debug (fun m ->
              m "Fallback to the default route (exn: %s)"
                (Printexc.to_string exn));
          let[@warning "-8"] (Some request) = request.extract None Any in
          default request target
      end
*)
