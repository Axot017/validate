open Ppxlib
open Exps
open Simple_type

type number = Int of int | Float of float [@@deriving show]

let process_numeric_attribute = function
  | Pconst_integer (i, _) -> Int (int_of_string i)
  | Pconst_float (f, _) -> Float (float_of_string f)
  | _ -> Location.raise_errorf "Attribute must be an integer or float"

let number_attribute name context =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    context
    Ast_pattern.(single_expr_payload (pexp_constant __))
    process_numeric_attribute

let int_attrribute name context =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    context
    Ast_pattern.(single_expr_payload (eint __))
    (fun x -> x)

let string_attrribute name context =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    context
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let unit_attribute name context =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    context
    Ast_pattern.(pstr nil)
    ()

type 'a validator_params = {
  name : string;
  build_exp : 'a -> loc_type -> expression option;
}

let get_exp attr (build_exp : 'a -> expression) v loc_type =
  Attribute.get attr v |> Option.map (fun x -> build_exp (x, loc_type))

let length_exp f =
  match f.typ with
  | String -> module_ident_exp ~loc:f.loc "String" "length"
  | List _ -> module_ident_exp ~loc:f.loc "List" "length"
  | _ ->
      Location.raise_errorf ~loc:f.loc "length is not supported for this type"

let validate_number_func_exp ~loc name value =
  match value with
  | Int i ->
      validate_func_exp name ~loc
        [
          (Nolabel, simple_ident_exp ~loc "string_of_int"); (Nolabel, int_exp i);
        ]
  | Float f ->
      validate_func_exp name ~loc
        [
          (Nolabel, simple_ident_exp ~loc "string_of_float");
          (Nolabel, float_exp f);
        ]

let validators ctx =
  [
    {
      name = "min_length";
      build_exp =
        get_exp (int_attrribute "min_length" ctx) (fun (x, loc_type) ->
            validate_func_exp "validate_min_length" ~loc:loc_type.loc
              [ (Nolabel, length_exp loc_type); (Nolabel, int_exp x) ]);
    };
    {
      name = "max_length";
      build_exp =
        get_exp (int_attrribute "max_length" ctx) (fun (x, loc_type) ->
            validate_func_exp "validate_max_length" ~loc:loc_type.loc
              [ (Nolabel, length_exp loc_type); (Nolabel, int_exp x) ]);
    };
    {
      name = "length_equals";
      build_exp =
        get_exp (int_attrribute "length_equals" ctx) (fun (x, loc_type) ->
            validate_func_exp "validate_length_equals" ~loc:loc_type.loc
              [ (Nolabel, length_exp loc_type); (Nolabel, int_exp x) ]);
    };
    {
      name = "url";
      build_exp =
        get_exp (unit_attribute "url" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_url" ~loc:loc_type.loc []);
    };
    {
      name = "uuid";
      build_exp =
        get_exp (unit_attribute "uuid" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_uuid" ~loc:loc_type.loc []);
    };
    {
      name = "email";
      build_exp =
        get_exp (unit_attribute "email" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_email" ~loc:loc_type.loc []);
    };
    {
      name = "numeric";
      build_exp =
        get_exp (unit_attribute "numeric" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_numeric" ~loc:loc_type.loc []);
    };
    {
      name = "alpha";
      build_exp =
        get_exp (unit_attribute "alpha" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_alpha" ~loc:loc_type.loc []);
    };
    {
      name = "alphanumeric";
      build_exp =
        get_exp (unit_attribute "alphanumeric" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_alphanumeric" ~loc:loc_type.loc []);
    };
    {
      name = "lowercase";
      build_exp =
        get_exp (unit_attribute "lowercase" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_lowercase" ~loc:loc_type.loc []);
    };
    {
      name = "lowercase_alphanumeric";
      build_exp =
        get_exp (unit_attribute "lowercase_alphanumeric" ctx)
          (fun (_, loc_type) ->
            validate_func_exp "validate_lowercase_alphanumeric"
              ~loc:loc_type.loc []);
    };
    {
      name = "uppercase";
      build_exp =
        get_exp (unit_attribute "uppercase" ctx) (fun (_, loc_type) ->
            validate_func_exp "validate_uppercase" ~loc:loc_type.loc []);
    };
    {
      name = "uppercase_alphanumeric";
      build_exp =
        get_exp (unit_attribute "uppercase_alphanumeric" ctx)
          (fun (_, loc_type) ->
            validate_func_exp "validate_uppercase_alphanumeric"
              ~loc:loc_type.loc []);
    };
    {
      name = "less_than";
      build_exp =
        get_exp (number_attribute "less_than" ctx) (fun (x, loc_type) ->
            validate_number_func_exp ~loc:loc_type.loc "validate_less_than" x);
    };
    {
      name = "less_than_or_equal";
      build_exp =
        get_exp (number_attribute "less_than_or_equal" ctx)
          (fun (x, loc_type) ->
            validate_number_func_exp ~loc:loc_type.loc
              "validate_less_than_or_equal" x);
    };
    {
      name = "greater_than";
      build_exp =
        get_exp (number_attribute "greater_than" ctx) (fun (x, loc_type) ->
            validate_number_func_exp ~loc:loc_type.loc "validate_greater_than" x);
    };
    {
      name = "greater_than_or_equal";
      build_exp =
        get_exp (number_attribute "greater_than_or_equal" ctx)
          (fun (x, loc_type) ->
            validate_number_func_exp ~loc:loc_type.loc
              "validate_greater_than_or_equal" x);
    };
    {
      name = "equal_to";
      build_exp =
        get_exp (number_attribute "equal_to" ctx) (fun (x, loc_type) ->
            validate_number_func_exp ~loc:loc_type.loc "validate_equal_to" x);
    };
    {
      name = "not_equal_to";
      build_exp =
        get_exp (number_attribute "not_equal_to" ctx) (fun (x, loc_type) ->
            validate_number_func_exp ~loc:loc_type.loc "validate_not_equal_to" x);
    };
    {
      name = "regex";
      build_exp =
        get_exp (string_attrribute "regex" ctx) (fun (x, loc_type) ->
            validate_func_exp "validate_str_regex" ~loc:loc_type.loc
              [ (Nolabel, string_exp ~loc:loc_type.loc x) ]);
    };
  ]

let ct_validators = validators Attribute.Context.core_type
let ld_validators = validators Attribute.Context.label_declaration
let ct_dive_attribute = unit_attribute "dive" Attribute.Context.core_type
let ct_divable ct = Attribute.get ct_dive_attribute ct |> Option.is_some

let rec cts_has_recursive cts searched_type =
  let loc_types = List.map extract_loc_type cts in
  let cts_to_loc_types = List.combine cts loc_types in
  let recursive (ct, loc_type) =
    match loc_type.typ with
    | List (_, ct) -> cts_has_recursive [ ct ] searched_type
    | Option (_, ct) -> cts_has_recursive [ ct ] searched_type
    | Tuple t ->
        t |> List.map (fun (_, ct) -> ct) |> fun cts ->
        cts_has_recursive cts searched_type
    | Other type_name -> (
        match type_name with
        | Lident name ->
            let same_type = name = searched_type in
            let divable = ct_divable ct in
            same_type && divable
        | _ -> false)
    | _ -> false
  in

  cts_to_loc_types |> List.exists recursive

let ld_dive_attribute =
  unit_attribute "dive" Attribute.Context.label_declaration

let ld_divable ld = Attribute.get ld_dive_attribute ld |> Option.is_some

let ct_validators_to_apply ct loc_type =
  ct_validators |> List.filter_map (fun v -> v.build_exp ct loc_type)

let ld_validators_to_apply ld loc_type =
  ld_validators |> List.filter_map (fun v -> v.build_exp ld loc_type)

let lds_has_recursive lds searched_type =
  let loc_types = List.map extract_record_field lds in
  let lds_to_loc_types = List.combine lds loc_types in
  let recursive (ld, record_field) =
    match record_field.loc_type.typ with
    | List (_, ct) -> cts_has_recursive [ ct ] searched_type
    | Option (_, ct) -> cts_has_recursive [ ct ] searched_type
    | Tuple t ->
        t |> List.map (fun (_, ct) -> ct) |> fun cts ->
        cts_has_recursive cts searched_type
    | Other type_name -> (
        match type_name with
        | Lident name ->
            let same_type = name = searched_type in
            let ld_divable = ld_divable ld in
            let ct_divable = ct_divable ld.pld_type in
            let divable = ld_divable || ct_divable in
            same_type && divable
        | _ -> false)
    | _ -> false
  in

  lds_to_loc_types |> List.exists recursive

let rec validators_list_exp ~validators ~divable loc_type =
  match loc_type.typ with
  | List (t, inner_type) ->
      let inner_divable = ct_divable inner_type in
      let inner_loc_type = { loc_type with typ = t } in
      let inner_validators = ct_validators_to_apply inner_type inner_loc_type in
      let deep_validators_exp =
        inner_loc_type
        |> validators_list_exp ~validators:inner_validators
             ~divable:inner_divable
        |> validate_list_exp ~loc:loc_type.loc
      in
      let all_validators = deep_validators_exp :: validators in
      list_exp ~loc:loc_type.loc all_validators
  | Other type_name -> (
      match divable with
      | true ->
          list_exp ~loc:loc_type.loc
            [
              ignore_ok_exp ~loc:loc_type.loc
              @@ dive_exp ~loc:loc_type.loc type_name;
            ]
      | false -> list_exp ~loc:loc_type.loc [])
  | Tuple types ->
      let args_count = List.length types in
      let tuple_extractor_exp = tuple_element_extractor_fun_exp args_count in
      let indexes = List.init args_count (fun i -> i) in
      let indexed_types = List.combine indexes types in
      let mapper (i, (t, ct)) =
        let inner_type = { loc_type with typ = t } in
        let inner_validators = ct_validators_to_apply ct inner_type in
        let inner_divable = ct_divable ct in
        inner_type
        |> validators_list_exp ~validators:inner_validators
             ~divable:inner_divable
        |> validate_field_exp ~loc:ct.ptyp_loc (string_of_int i)
             (tuple_extractor_exp ~loc:ct.ptyp_loc i)
      in
      let body =
        indexed_types |> List.map mapper |> list_exp ~loc:loc_type.loc
        |> validate_keyed_exp ~loc:loc_type.loc
      in

      list_exp ~loc:loc_type.loc [ body ]
  | Option (t, inner_ct) ->
      let inner_type = { loc_type with typ = t } in

      let inner_validators = ct_validators_to_apply inner_ct inner_type in
      let inner_divable = ct_divable inner_ct in
      let validators =
        inner_type
        |> validators_list_exp ~divable:inner_divable
             ~validators:inner_validators
        |> validate_option ~loc:loc_type.loc
      in
      list_exp ~loc:loc_type.loc [ validators ]
  | _ -> validators |> list_exp ~loc:loc_type.loc

let type_validator_exp (ct : core_type) =
  let loc_type = extract_loc_type ct in
  let validators = ct_validators_to_apply ct loc_type in
  let divable = ct_divable ct in
  validators_list_exp ~validators ~divable loc_type
  |> validate_group_exp ~loc:ct.ptyp_loc

let field_validator_exp (ld : label_declaration) =
  let f = extract_record_field ld in
  let divable_ld = ld_divable ld in
  let divable_ct = ct_divable ld.pld_type in
  let divable = divable_ld || divable_ct in
  let ld_validators = ld_validators_to_apply ld f.loc_type in
  let ct_validators = ct_validators_to_apply ld.pld_type f.loc_type in
  let validators = ld_validators @ ct_validators in
  f.loc_type
  |> validators_list_exp ~validators ~divable
  |> validate_field_exp ~loc:ld.pld_loc f.name
       (field_extractor_exp ~loc:f.loc_type.loc f.name)

let validate_record_exp ~loc label_declarations =
  label_declarations
  |> List.map field_validator_exp
  |> list_exp ~loc |> validate_keyed_exp ~loc |> validate_exp ~loc

let validate_abstract_exp ~loc ct =
  ct |> type_validator_exp |> validate_exp ~loc

let validate_variant_tuple_exp ~variant_name cts =
  let args_count = List.length cts in
  let tuple_extractor_exp =
    variant_tuple_extractor_exp variant_name args_count
  in
  let indexes = List.init args_count (fun i -> i) in
  let indexed_types = List.combine indexes cts in
  let mapper (i, ct) =
    let inner_type = extract_loc_type ct in
    let inner_divable = ct_divable ct in

    let inner_validators = ct_validators_to_apply ct inner_type in
    match (List.length inner_validators, inner_divable) with
    | 0, false -> None
    | _ ->
        let name = Printf.sprintf "%s.%s" variant_name (string_of_int i) in
        inner_type
        |> validators_list_exp ~validators:inner_validators
             ~divable:inner_divable
        |> validate_named_value_exp ~loc:ct.ptyp_loc name
             (tuple_extractor_exp ~loc:ct.ptyp_loc i)
        |> Option.some
  in
  indexed_types |> List.filter_map mapper

let validate_variant_record_exp ~variant_name lds =
  let field_names = List.map (fun ld -> ld.pld_name.txt) lds in
  let variant_extractor_exp =
    variant_record_extractor_exp variant_name field_names
  in
  let mapper ld =
    let inner_type = extract_record_field ld in
    let field_name = ld.pld_name.txt in
    let name = Printf.sprintf "%s.%s" variant_name ld.pld_name.txt in
    let divable_ld = ld_divable ld in
    let divable_ct = ct_divable ld.pld_type in
    let divable = divable_ld || divable_ct in
    let ld_validators = ld_validators_to_apply ld inner_type.loc_type in
    let ct_validators =
      ct_validators_to_apply ld.pld_type inner_type.loc_type
    in
    let validators = ld_validators @ ct_validators in
    match (List.length validators, divable) with
    | 0, false -> None
    | _ ->
        inner_type.loc_type
        |> validators_list_exp ~validators ~divable
        |> validate_named_value_exp ~loc:ld.pld_loc name
             (variant_extractor_exp ~loc:ld.pld_loc field_name)
        |> Option.some
  in
  lds |> List.filter_map mapper

let validate_constructor_declaration_exp cd =
  match cd.pcd_args with
  | Pcstr_tuple cts ->
      validate_variant_tuple_exp ~variant_name:cd.pcd_name.txt cts
  | Pcstr_record lds ->
      validate_variant_record_exp ~variant_name:cd.pcd_name.txt lds

let validate_variant_exp ~loc cds =
  cds
  |> List.map validate_constructor_declaration_exp
  |> List.flatten |> list_exp ~loc |> validate_keyed_exp ~loc
  |> validate_exp ~loc
