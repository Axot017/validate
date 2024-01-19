open Ppxlib
open Ast_helper
open Field

let int_exp i = Exp.constant (Pconst_integer (string_of_int i, None))
let float_exp f = Exp.constant (Pconst_float (string_of_float f, None))
let simple_ident_exp ~loc str = Exp.ident { txt = Lident str; loc }

let module_ident_exp ~loc m str =
  Exp.(ident { txt = Ldot (Lident m, str); loc })

let rec list_exp ~loc = function
  | [] -> Exp.construct { txt = Lident "[]"; loc } None
  | x :: xs ->
      Exp.construct { txt = Lident "::"; loc }
        (Some (Exp.tuple [ x; list_exp ~loc xs ]))

let length_exp f =
  match f.typ with
  | String -> module_ident_exp ~loc:f.loc "String" "length"
  | List _ -> module_ident_exp ~loc:f.loc "List" "length"
  | _ ->
      Location.raise_errorf ~loc:f.loc "length is not supported for this type"

let validate_func_exp ~loc validator_name params =
  let open Exp in
  match params with
  | [] -> module_ident_exp ~loc "Validate" validator_name
  | _ -> apply (module_ident_exp ~loc "Validate" validator_name) params

let field_extractor_exp ~loc name =
  let open Exp in
  fun_ Nolabel None
    (Pat.var { txt = "x"; loc })
    (field (simple_ident_exp ~loc "x") { txt = Lident name; loc })

let validate_list_exp ~loc inner =
  let open Exp in
  apply (module_ident_exp ~loc "Validate" "list") [ (Nolabel, inner) ]

let ignore_ok_exp ~loc inner =
  let open Exp in
  apply (module_ident_exp ~loc "Validate" "ignore_ok") [ (Nolabel, inner) ]

let dive_exp ~loc type_name =
  let open Exp in
  let txt =
    match type_name with
    | Lident name -> Lident (Printf.sprintf "validate_%s" name)
    | Ldot (module_name, name) ->
        Ldot (module_name, Printf.sprintf "validate_%s" name)
    | _ -> Location.raise_errorf ~loc "Something went wrong"
  in

  ident { txt; loc }
