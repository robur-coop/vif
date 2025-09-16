#require "vif";;

(* This is a bad example of a [_ Vif.Uri.atom] as the converter raises *)
let mouth =
  Vif.Uri.conv
    (function "mouth" -> `Mouth | _ -> invalid_arg "not a mouth")
    (fun `Mouth -> "mouth")
    (Vif.Uri.string `Query_value)

let url = Vif.Uri.(rel /% string `Path /?? ("q", mouth) ** nil)

let str = "/horse?q=mouth"
and str' = "/horse/head?q=mouth&p=hoofs"
and str'' = "/horse?q=hoofs"
;;

(* We can write simple tests to see if the uri regular expression matches *)
Fmt.pr "%S matches: %B\n%!" str (Vif.Uri.execp url str);
Fmt.pr "%S matches: %B\n%!" str' (Vif.Uri.execp url str');
;;

(* And we can write tests that extract the values of a sample url *)
let fn p `Mouth =
  Fmt.str "Don't look a %s in the mouth." p

let error = function
  | `No_match -> "No match."
  | `Converter_failure exn -> Fmt.str "`Converter failure: %a" Fmt.exn exn

let run uri str =
  Vif.Uri.extract uri str fn
  |> Result.fold ~ok:Fun.id ~error
  |> print_endline

let () =
  let () =
    run url str;
    run url str';
    run url str''
  in
  let url = Vif.Uri.(rel /% string `Path /?? ("q", mouth) ** any) in
  Vif.Uri.eval_additional_queries url ["q", ["hoofs"]] "horse" `Mouth
  |> print_endline
;;
