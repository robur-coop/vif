type 'a atom = 'a Tyre.Internal.wit

let atom re = Tyre.Internal.build re
let slash = Re.char '/'
let comma = Re.char ','

let list ?m ~component n re =
  let open Re in
  match component with
  | `Path -> repn (seq [ slash; re ]) n m
  | `Query_value ->
      if n = 0 then alt [ epsilon; seq [ re; repn (seq [ comma; re ]) 0 m ] ]
      else seq [ re; repn (seq [ comma; re ]) (n - 1) m ]

let atom_path : type a. int -> a Tyre.Internal.raw -> int * a atom * Re.t =
  let open Re in
  fun i -> function
    | Rep e ->
        let _, w, re = atom 1 e in
        ( i + 1
        , Rep (i, w, Re.compile re)
        , group (list ~component:`Path 0 (no_group re)) )
    | Opt e ->
        let i', w, re = atom i e in
        let id, re = mark re in
        (i', Opt (id, w), seq [ alt [ epsilon; seq [ slash; re ] ] ])
    | e ->
        let i', w, re = atom i e in
        (i', w, seq [ slash; re ])
