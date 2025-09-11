#require "vif";;

(* We have to thunk the uri due to OCaml value restriction *)
let url () =
  (* This is a bad example of a [_ Vif.Uri.atom] as the converter raises *)
  let mouth =
    Vif.Uri.conv
      (function "mouth" -> `Mouth | _ -> invalid_arg "not a mouth")
      (fun `Mouth -> "mouth")
      (Vif.Uri.string `Query_value)
  in
  Vif.Uri.(rel /% string `Path /?? ("q", mouth) ** nil)

let str = "/horse?q=mouth"
and str' = "/horse/head?q=mouth&p=hoofs"
and str'' = "/horse?q=hoofs"
;;

(* We can write simple tests to see if the uri regular expression matches *)
Fmt.pr "%S matches: %B\n%!" str (Vif.Uri.execp (url ()) str);
Fmt.pr "%S matches: %B\n%!" str' (Vif.Uri.execp (url ()) str');
;;

(* And we can write tests that extract the values of a sample url *)
let fn p `Mouth =
  Fmt.str "Don't look a %s in the mouth." p

let error = function
  | `NoMatch -> "No match."
  | `ConverterFailure exn -> Fmt.str "`ConverterFailure %a" Fmt.exn exn
;;

Vif.Uri.extract (url ()) str fn
|> Result.fold ~ok:Fun.id ~error
|> print_endline
;;

Vif.Uri.extract (url ()) str' fn
|> Result.fold ~ok:Fun.id ~error
|> print_endline
;;

Vif.Uri.extract (url ()) str'' fn
|> Result.fold ~ok:Fun.id ~error
|> print_endline
;;
