open Ppxlib
open Ast_helper
open Field
open Utils

let min_length_key = "min_length"
let max_length_key = "max_length"
let url_key = "url"
let uuid_key = "uuid"
let numeric_key = "numeric"
let alpha_key = "alpha"
let alphanumeric_key = "alphanumeric"
let lowercase_key = "lowercase"
let lowercase_alphanumeric_key = "lowercase_alphanumeric"
let uppercase_key = "uppercase"
let uppercase_alphanumeric_key = "uppercase_alphanumeric"
let less_than = "less_than"
let less_than_or_equal = "less_than_or_equal"
let greater_than = "greater_than"
let greater_than_or_equal = "greater_than_or_equal"
let equal_to = "equal_to"
let not_equal_to = "not_equal_to"
let email = "email"
let regex = "regex"
let list_min_length_key = "list_min_length"
let list_max_length_key = "list_max_length"
let dive = "dive"

type list_validator = ListMinLength of int | ListMaxLength of int
[@@deriving show]

type validator =
  | MinLength of int
  | MaxLength of int
  | Url
  | Uuid
  | Numeric
  | Alpha
  | Alphanumeric
  | Lowercase
  | LowercaseAlphanumeric
  | Uppercase
  | UppercaseAlphanumeric
  | LessThan of number
  | LessThanOrEqual of number
  | GreaterThan of number
  | GreaterThanOrEqual of number
  | EqualTo of number
  | NotEqualTo of number
  | Email
  | Regex of string
[@@deriving show]

and number = Int of int | Float of float [@@deriving show]
and validators = validator list [@@deriving show]

let string_of_validator = function
  | MinLength _ -> min_length_key
  | MaxLength _ -> max_length_key
  | Url -> url_key
  | Uuid -> uuid_key
  | Numeric -> numeric_key
  | Alpha -> alpha_key
  | Alphanumeric -> alphanumeric_key
  | Lowercase -> lowercase_key
  | LowercaseAlphanumeric -> lowercase_alphanumeric_key
  | Uppercase -> uppercase_key
  | UppercaseAlphanumeric -> uppercase_alphanumeric_key
  | LessThan _ -> less_than
  | LessThanOrEqual _ -> less_than_or_equal
  | GreaterThan _ -> greater_than
  | GreaterThanOrEqual _ -> greater_than_or_equal
  | EqualTo _ -> equal_to
  | NotEqualTo _ -> not_equal_to
  | Email -> email
  | Regex _ -> regex

let process_numeric_attribute ?loc = function
  | Pconst_integer (i, _) -> Int (int_of_string i)
  | Pconst_float (f, _) -> Float (float_of_string f)
  | _ -> Location.raise_errorf ?loc "Attribute must be an integer or float"

let number_attribute ?loc name =
  Attribute.declare name Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (pexp_constant __))
    (process_numeric_attribute ?loc)

let int_attrribute name =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    (fun x -> x)

let string_attrribute name =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let unit_attribute name =
  Attribute.declare
    Printf.(sprintf "ppx_derive_validator.%s" name)
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let min_length_attribute = int_attrribute min_length_key
let max_length_attribute = int_attrribute max_length_key
let uri_attribute = unit_attribute url_key
let uuid_attribute = unit_attribute uuid_key
let numeric_attribute = unit_attribute numeric_key
let alpha_attribute = unit_attribute alpha_key
let alphanumeric_attribute = unit_attribute alphanumeric_key
let lowercase_attribute = unit_attribute lowercase_key
let lowercase_alphanumeric_attribute = unit_attribute lowercase_alphanumeric_key
let uppercase_attribute = unit_attribute uppercase_key
let uppercase_alphanumeric_attribute = unit_attribute uppercase_alphanumeric_key
let less_than_attribute = number_attribute less_than
let less_than_or_equal_attribute = number_attribute less_than_or_equal
let greater_than_attribute = number_attribute greater_than
let greater_than_or_equal_attribute = number_attribute greater_than_or_equal
let equal_to_attribute = number_attribute equal_to
let not_equal_to_attribute = number_attribute not_equal_to
let list_min_length_attribute = int_attrribute list_min_length_key
let list_max_length_attribute = int_attrribute list_max_length_key
let dive_attribute = unit_attribute dive
let email_attribute = unit_attribute email
let regex_attribute = string_attrribute regex

let extract_list_validators (ld : label_declaration) =
  [
    Attribute.get list_min_length_attribute ld
    |> Option.map (fun x -> ListMinLength x);
    Attribute.get list_max_length_attribute ld
    |> Option.map (fun x -> ListMaxLength x);
  ]
  |> List.filter_map (fun x -> x)

let extract_validators (ld : label_declaration) =
  [
    Attribute.get min_length_attribute ld |> Option.map (fun x -> MinLength x);
    Attribute.get max_length_attribute ld |> Option.map (fun x -> MaxLength x);
    Attribute.get uri_attribute ld |> Option.map (fun _ -> Url);
    Attribute.get uuid_attribute ld |> Option.map (fun _ -> Uuid);
    Attribute.get numeric_attribute ld |> Option.map (fun _ -> Numeric);
    Attribute.get alpha_attribute ld |> Option.map (fun _ -> Alpha);
    Attribute.get alphanumeric_attribute ld
    |> Option.map (fun _ -> Alphanumeric);
    Attribute.get lowercase_attribute ld |> Option.map (fun _ -> Lowercase);
    Attribute.get lowercase_alphanumeric_attribute ld
    |> Option.map (fun _ -> LowercaseAlphanumeric);
    Attribute.get uppercase_attribute ld |> Option.map (fun _ -> Uppercase);
    Attribute.get uppercase_alphanumeric_attribute ld
    |> Option.map (fun _ -> UppercaseAlphanumeric);
    Attribute.get less_than_attribute ld |> Option.map (fun x -> LessThan x);
    Attribute.get less_than_or_equal_attribute ld
    |> Option.map (fun x -> LessThanOrEqual x);
    Attribute.get greater_than_attribute ld
    |> Option.map (fun x -> GreaterThan x);
    Attribute.get greater_than_or_equal_attribute ld
    |> Option.map (fun x -> GreaterThanOrEqual x);
    Attribute.get equal_to_attribute ld |> Option.map (fun x -> EqualTo x);
    Attribute.get not_equal_to_attribute ld
    |> Option.map (fun x -> NotEqualTo x);
    Attribute.get email_attribute ld |> Option.map (fun _ -> Email);
    Attribute.get regex_attribute ld |> Option.map (fun x -> Regex x);
  ]
  |> List.filter_map (fun x -> x)

let length_ident f =
  match f.field_type with
  | String ->
      Exp.(ident { txt = Ldot (Lident "String", "length"); loc = f.loc })
  | List _ -> Exp.(ident { txt = Ldot (Lident "List", "length"); loc = f.loc })
  | _ ->
      Location.raise_errorf ~loc:f.loc "length is not supported for this type"

let validator_exp_template ~loc validator_name params =
  let open Exp in
  match params with
  | [] -> ident { txt = Ldot (Lident "Validate", validator_name); loc }
  | _ ->
      apply
        (ident { txt = Ldot (Lident "Validate", validator_name); loc })
        params

let max_length_validator_exp max record_field =
  validator_exp_template "validate_max_length" ~loc:record_field.loc
    [
      (Nolabel, length_ident record_field);
      (Nolabel, Exp.constant (Pconst_integer (string_of_int max, None)));
    ]

let min_length_validator_exp min record_field =
  validator_exp_template "validate_min_length" ~loc:record_field.loc
    [
      (Nolabel, length_ident record_field);
      (Nolabel, Exp.constant (Pconst_integer (string_of_int min, None)));
    ]

let url_validator_exp record_field =
  validator_exp_template "validate_url" ~loc:record_field.loc []

let uuid_validator_exp record_field =
  validator_exp_template "validate_uuid" ~loc:record_field.loc []

let numeric_validator_exp record_field =
  validator_exp_template "validate_numeric" ~loc:record_field.loc []

let alpha_validator_exp record_field =
  validator_exp_template "validate_alpha" ~loc:record_field.loc []

let alphanumeric_validator_exp record_field =
  validator_exp_template "validate_alphanumeric" ~loc:record_field.loc []

let lowercase_validator_exp record_field =
  validator_exp_template "validate_lowercase" ~loc:record_field.loc []

let lowercase_alphanumeric_validator_exp record_field =
  validator_exp_template "validate_lowercase_alphanumeric" ~loc:record_field.loc
    []

let uppercase_validator_exp record_field =
  validator_exp_template "validate_uppercase" ~loc:record_field.loc []

let uppercase_alphanumeric_validator_exp record_field =
  validator_exp_template "validate_uppercase_alphanumeric" ~loc:record_field.loc
    []

let regex_validator_exp regex record_field =
  validator_exp_template "validate_str_regex" ~loc:record_field.loc
    [ (Nolabel, Exp.constant (Pconst_string (regex, record_field.loc, None))) ]

let number_to_exp = function
  | Int i -> Exp.constant (Pconst_integer (string_of_int i, None))
  | Float f -> Exp.constant (Pconst_float (string_of_float f, None))

let number_to_str_exp ~loc = function
  | Int _ -> Exp.ident { txt = Lident "string_of_int"; loc }
  | Float _ -> Exp.ident { txt = Lident "string_of_float"; loc }

let less_than_validator_exp number record_field =
  validator_exp_template "validate_less_than" ~loc:record_field.loc
    [
      (Nolabel, number_to_str_exp ~loc:record_field.loc number);
      (Nolabel, number_to_exp number);
    ]

let less_than_or_equal_validator_exp number record_field =
  validator_exp_template "validate_less_than_or_equal" ~loc:record_field.loc
    [
      (Nolabel, number_to_str_exp ~loc:record_field.loc number);
      (Nolabel, number_to_exp number);
    ]

let greater_than_validator_exp number record_field =
  validator_exp_template "validate_greater_than" ~loc:record_field.loc
    [
      (Nolabel, number_to_str_exp ~loc:record_field.loc number);
      (Nolabel, number_to_exp number);
    ]

let greater_than_or_equal_validator_exp number record_field =
  validator_exp_template "validate_greater_than_or_equal" ~loc:record_field.loc
    [
      (Nolabel, number_to_str_exp ~loc:record_field.loc number);
      (Nolabel, number_to_exp number);
    ]

let equal_to_validator_exp number record_field =
  validator_exp_template "validate_equal_to" ~loc:record_field.loc
    [
      (Nolabel, number_to_str_exp ~loc:record_field.loc number);
      (Nolabel, number_to_exp number);
    ]

let not_equal_to_validator_exp number record_field =
  validator_exp_template "validate_not_equal_to" ~loc:record_field.loc
    [
      (Nolabel, number_to_str_exp ~loc:record_field.loc number);
      (Nolabel, number_to_exp number);
    ]

let option_validator_exp record_field inner =
  validator_exp_template "option" ~loc:record_field.loc [ (Nolabel, inner) ]

let email_validator_exp record_field =
  validator_exp_template "validate_email" ~loc:record_field.loc []

let rec validator_exp record_field validator =
  match record_field.field_type with
  | Bool | Int | Float | String -> (
      match validator with
      | MaxLength max -> max_length_validator_exp max record_field
      | MinLength min -> min_length_validator_exp min record_field
      | Url -> url_validator_exp record_field
      | Uuid -> uuid_validator_exp record_field
      | Numeric -> numeric_validator_exp record_field
      | Alpha -> alpha_validator_exp record_field
      | Alphanumeric -> alphanumeric_validator_exp record_field
      | Lowercase -> lowercase_validator_exp record_field
      | LowercaseAlphanumeric ->
          lowercase_alphanumeric_validator_exp record_field
      | Uppercase -> uppercase_validator_exp record_field
      | UppercaseAlphanumeric ->
          uppercase_alphanumeric_validator_exp record_field
      | LessThan number -> less_than_validator_exp number record_field
      | LessThanOrEqual number ->
          less_than_or_equal_validator_exp number record_field
      | GreaterThan number -> greater_than_validator_exp number record_field
      | GreaterThanOrEqual number ->
          greater_than_or_equal_validator_exp number record_field
      | EqualTo number -> equal_to_validator_exp number record_field
      | NotEqualTo number -> not_equal_to_validator_exp number record_field
      | Email -> email_validator_exp record_field
      | Regex regex -> regex_validator_exp regex record_field)
  | Option inner_record_field_type ->
      option_validator_exp record_field
        (validator_exp
           { record_field with field_type = inner_record_field_type }
           validator)
  | _ -> Location.raise_errorf ~loc:record_field.loc "Something went wrong"

let field_extractor_exp f =
  let open Exp in
  fun_ Nolabel None
    (Pat.var { txt = "x"; loc = f.loc })
    (field
       (ident { txt = Lident "x"; loc = f.loc })
       { txt = Lident f.name; loc = f.loc })

let list_validator_exp ~loc inner =
  let open Exp in
  apply
    (ident { txt = Ldot (Lident "Validate", "list"); loc })
    [ (Nolabel, inner) ]

let list_specific_validator_exp record_field list_validator =
  match list_validator with
  | ListMinLength min ->
      validator_exp_template "validate_min_length" ~loc:record_field.loc
        [
          (Nolabel, length_ident record_field);
          (Nolabel, Exp.constant (Pconst_integer (string_of_int min, None)));
        ]
  | ListMaxLength max ->
      validator_exp_template "validate_max_length" ~loc:record_field.loc
        [
          (Nolabel, length_ident record_field);
          (Nolabel, Exp.constant (Pconst_integer (string_of_int max, None)));
        ]

let ignored_exp ~loc inner =
  let open Exp in
  apply
    (ident { txt = Ldot (Lident "Validate", "ignore_ok"); loc })
    [ (Nolabel, inner) ]

let call_other_type_validator_exp ~loc type_name =
  let open Exp in
  let txt =
    match type_name with
    | Lident name -> Lident (Printf.sprintf "validate_%s" name)
    | Ldot (module_name, name) ->
        Ldot (module_name, Printf.sprintf "validate_%s" name)
    | _ -> Location.raise_errorf ~loc "Something went wrong"
  in

  ident { txt; loc }

let rec field_validators_list_exp f (ld : label_declaration) =
  match f.field_type with
  | List t ->
      let list_validators =
        extract_list_validators ld |> List.map (list_specific_validator_exp f)
      in
      expr_list f.loc
        (list_validators
        @ [
            list_validator_exp ~loc:f.loc
            @@ field_validators_list_exp { f with field_type = t } ld;
          ])
  | Other type_name ->
      let divable = Attribute.get dive_attribute ld |> Option.is_some in
      if divable then
        expr_list f.loc
          [
            ignored_exp ~loc:f.loc
            @@ call_other_type_validator_exp ~loc:f.loc type_name;
          ]
      else expr_list f.loc []
  | _ ->
      let generator = validator_exp f in
      let validators = extract_validators ld in
      let exps = validators |> List.map generator in
      expr_list f.loc exps

let field_validator_exp (ld : label_declaration) =
  let open Exp in
  let f = extract_record_field ld in
  apply
    (ident { txt = Ldot (Lident "Validate", "field"); loc = f.loc })
    [
      (Nolabel, constant (Pconst_string (f.name, f.loc, None)));
      (Nolabel, field_extractor_exp f);
      (Nolabel, field_validators_list_exp f ld);
    ]
