let src = Logs.Src.create "vif.r"

module Log = (val Logs.src_log src : Logs.LOG)

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
        ( i + 1
        , Rep (i, w, Re.compile re)
        , group (list ~component:`Query_value 0 (no_group re)) )
    | e -> atom i e

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
      let grps, wa, ra = atom_query 0 (Tyre.Internal.from_t a) in
      let total_grps, wq, b_any, rq = collect_query q in
      let total_grps = total_grps + grps in
      (total_grps, Cons (wa, wq), b_any, (s, (ra, grps)) :: rq)

let rec shift_literals : type a. int -> a atom -> a atom =
 fun shift -> function
  | Tyre.Internal.Lit i -> Lit (i + shift)
  | Conv (x, f) -> Conv (shift_literals shift x, f)
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

let sort_query = List.sort (fun (x, _) (y, _) -> String.compare x y)

let query current_idx q =
  let grps, wq, b, rql = collect_query q in
  let rel = sort_query rql in
  let p = build_permutation current_idx (fun (_, (_, i)) -> i) rql rel in
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
        if not any_query then fun x -> x else fun l -> Re.(rep any) :: l
      in
      let re =
        let rel =
          let fn l (s, (re, _)) = Re.seq [ Re.str (s ^ "="); re ] :: l in
          List.fold_left fn [] rel
        in
        intersperse query_sep rel
        |> add_around_query
        |> List.rev
        |> add_around_query
      in
      let re = Re.seq (List.rev_append rp (end_path :: Re.char '?' :: re)) in
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

type ('fu, 'return) body =
  | Body :
      ('c, 'a) Vif_content_type.t
      -> (('c, 'a) Vif_request.t -> 'r, 'r) body

type 'r route = Route : ('f, 'x) body * ('x, 'r) Vif_u.t * 'f -> 'r route

let route body t f = Route (body, t, f)

type 'r re = Re : ('f, 'x) body * 'f * Re.Mark.t * ('x, 'r) t -> 'r re

let rec build_info_list idx = function
  | [] -> ([], [])
  | Route (b, t, f) :: l ->
      let idx, ret, re = url idx t in
      let rel, wl = build_info_list idx l in
      let id, re = Re.mark re in
      (re :: rel, Re (b, f, id, ret) :: wl)

type request = {
    extract: 'c 'a. ('c, 'a) Vif_content_type.t -> ('c, 'a) Vif_request.t option
}

let rec find_and_trigger : type r.
    original:string -> request:request -> Re.Group.t -> r re list -> r =
 fun ~original ~request subs -> function
  | [] -> raise Not_found
  | Re (Body body, f, id, ret) :: l ->
      if Re.Mark.test subs id then
        match request.extract body with
        | Some request -> extract ~original ret subs (f request)
        | None -> find_and_trigger ~original ~request subs l
      else find_and_trigger ~original ~request subs l

let dispatch : type r c.
       default:((c, string) Vif_request.t -> string -> r)
    -> r route list
    -> request:request
    -> target:string
    -> r =
 fun ~default l ->
  let rel, wl = build_info_list 1 l in
  let re = Re.(compile (whole_string (alt rel))) in
  fun ~request ~target ->
    match Re.exec_opt re target with
    | None ->
        Log.debug (fun m -> m "Fallback to the default route");
        let[@warning "-8"] (Some request) = request.extract Any in
        default request target
    | Some subs -> begin
        try find_and_trigger ~original:target ~request subs wl
        with exn ->
          Log.debug (fun m ->
              m "Fallback to the default route (exn: %s)"
                (Printexc.to_string exn));
          let[@warning "-8"] (Some request) = request.extract Any in
          default request target
      end
