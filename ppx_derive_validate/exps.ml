open Ppxlib
open Ast_helper
open Pats

let int_exp i = Exp.constant (Pconst_integer (string_of_int i, None))
let float_exp f = Exp.constant (Pconst_float (string_of_float f, None))
let string_exp ~loc s = Exp.constant (Pconst_string (s, loc, None))
let simple_ident_exp ~loc str = Exp.ident { txt = Lident str; loc }

let apply_exp f args =
  let open Exp in
  match args with [] -> f | _ -> apply f args

let module_ident_exp ~loc m str =
  Exp.(ident { txt = Ldot (Lident m, str); loc })

let rec list_exp ~loc = function
  | [] -> Exp.construct { txt = Lident "[]"; loc } None
  | x :: xs ->
      Exp.construct { txt = Lident "::"; loc }
        (Some (Exp.tuple [ x; list_exp ~loc xs ]))

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

let variant_tuple_extractor_exp ~loc expected_type_name total n =
  let pattern = n_element_tuple_pat ~loc ~prefix:"x" total in
  let match_exp =
    Exp.match_
      (simple_ident_exp ~loc "x")
      [
        Exp.case
          (Pat.construct
             { txt = Lident expected_type_name; loc }
             (Some pattern))
          (Exp.construct
             { txt = Lident "Some"; loc }
             (Some (simple_ident_exp ~loc (Printf.sprintf "x%d" n))));
        Exp.case (Pat.any ()) (Exp.construct { txt = Lident "None"; loc } None);
      ]
  in
  Exp.(fun_ Nolabel None (Pat.var { txt = "x"; loc }) match_exp)

let variant_record_extractor_exp ~loc expected_type_name field_names
    extracted_field_name =
  let pattern = record_pat ~loc field_names in
  let match_exp =
    Exp.match_
      (simple_ident_exp ~loc "x")
      [
        Exp.case
          (Pat.construct
             { txt = Lident expected_type_name; loc }
             (Some pattern))
          (Exp.construct
             { txt = Lident "Some"; loc }
             (Some (simple_ident_exp ~loc extracted_field_name)));
        Exp.case (Pat.any ()) (Exp.construct { txt = Lident "None"; loc } None);
      ]
  in
  Exp.(fun_ Nolabel None (Pat.var { txt = "x"; loc }) match_exp)

let validate_field_exp ~loc name extractor_fun_exp validators_list_exp =
  Exp.(
    apply
      (module_ident_exp ~loc "Validate" "field")
      [
        (Nolabel, string_exp ~loc name);
        (Nolabel, extractor_fun_exp);
        (Nolabel, validators_list_exp);
      ])

let validate_named_value_exp ~loc name extractor_fun_exp validators_list_exp =
  Exp.(
    apply
      (module_ident_exp ~loc "Validate" "named_value")
      [
        (Nolabel, string_exp ~loc name);
        (Nolabel, extractor_fun_exp);
        (Nolabel, validators_list_exp);
      ])

let tuple_element_extractor_fun_exp ~loc total n =
  let open Exp in
  let pattern = n_element_tuple_pat ~loc ~prefix:"x" total in
  fun_ Nolabel None pattern (simple_ident_exp ~loc (Printf.sprintf "x%d" n))

let validate_keyed_exp ~loc arg_exp =
  Exp.(apply (module_ident_exp ~loc "Validate" "keyed") [ (Nolabel, arg_exp) ])

let validate_group_exp ~loc arg_exp =
  Exp.(apply (module_ident_exp ~loc "Validate" "group") [ (Nolabel, arg_exp) ])

let validate_exp ~loc arg_exp =
  Exp.(
    apply
      (module_ident_exp ~loc "Validate" "validate")
      [ (Nolabel, arg_exp); (Nolabel, simple_ident_exp ~loc "x") ])

let validate_option ~loc arg_exp =
  Exp.(apply (module_ident_exp ~loc "Validate" "option") [ (Nolabel, arg_exp) ])

let ignore_if_func_exp ~loc (cond_exp : expression option) inner_exp =
  let open Exp in
  match cond_exp with
  | None -> inner_exp
  | Some cond_exp ->
      let cond_result_exp =
        apply cond_exp [ (Nolabel, simple_ident_exp ~loc "x") ]
      in
      apply
        (module_ident_exp ~loc "Validate" "ignore_if")
        [ (Nolabel, cond_result_exp); (Nolabel, inner_exp) ]
