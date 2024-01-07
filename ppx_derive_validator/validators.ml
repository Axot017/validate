open Ppxlib
open Ast_helper
open Field

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
  | [] -> ident { txt = Ldot (Lident "Validator", validator_name); loc }
  | _ ->
      apply
        (ident { txt = Ldot (Lident "Validator", validator_name); loc })
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
      | NotEqualTo number -> not_equal_to_validator_exp number record_field)
  | Option inner_record_field_type ->
      option_validator_exp record_field
        (validator_exp
           { record_field with field_type = inner_record_field_type }
           validator)
  | _ ->
      Location.raise_errorf ~loc:record_field.loc
        "Validator not supported for this type"

let check_if_validator_applicable validator record_field =
  match validator with
  | MinLength _ | MaxLength _ -> (
      match record_field.field_type with
      | String | List _ -> ()
      | _ ->
          Location.raise_errorf ~loc:record_field.loc
            "%s is not supported for this type"
            (string_of_validator validator))
  | Url | Uuid | Numeric | Alpha | Alphanumeric | Lowercase
  | LowercaseAlphanumeric | Uppercase | UppercaseAlphanumeric -> (
      match record_field.field_type with
      | String -> ()
      | _ ->
          Location.raise_errorf ~loc:record_field.loc
            "%s is not supported for this type"
            (string_of_validator validator))
  | LessThan _ | LessThanOrEqual _ | GreaterThan _ | GreaterThanOrEqual _
  | EqualTo _ | NotEqualTo _ -> (
      match record_field.field_type with
      | Int | Float -> ()
      | _ ->
          Location.raise_errorf ~loc:record_field.loc
            "%s is not supported for this type"
            (string_of_validator validator))
