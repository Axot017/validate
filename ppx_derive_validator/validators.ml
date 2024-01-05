open Ppxlib
open Ast_helper
open Field

type validator =
  | MinLength of int
  | MaxLength of int
  | MinValue of int
  | MaxValue of int
  | Required
  | Email
  | Uri
  | Uuid
  | Dive
[@@deriving show]

and validators = validator list [@@deriving show]

let min_length_attribute =
  Attribute.declare "ppx_derive_validator.min_length"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    (fun x -> x)

let max_length_attribute =
  Attribute.declare "ppx_derive_validator.max_length"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    (fun x -> x)

let min_value_attribute =
  Attribute.declare "ppx_derive_validator.min_value"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    (fun x -> x)

let max_value_attribute =
  Attribute.declare "ppx_derive_validator.max_value"
    Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    (fun x -> x)

let required_attribute =
  Attribute.declare "ppx_derive_validator.required"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let email_attribute =
  Attribute.declare "ppx_derive_validator.email"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let uri_attribute =
  Attribute.declare "ppx_derive_validator.uri"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let uuid_attribute =
  Attribute.declare "ppx_derive_validator.uuid"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let dive_attribute =
  Attribute.declare "ppx_derive_validator.dive"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let extract_validators (ld : label_declaration) =
  [
    Attribute.get min_length_attribute ld |> Option.map (fun x -> MinLength x);
    Attribute.get max_length_attribute ld |> Option.map (fun x -> MaxLength x);
    Attribute.get min_value_attribute ld |> Option.map (fun x -> MinValue x);
    Attribute.get max_value_attribute ld |> Option.map (fun x -> MaxValue x);
    Attribute.get required_attribute ld |> Option.map (fun _ -> Required);
    Attribute.get email_attribute ld |> Option.map (fun _ -> Email);
    Attribute.get uri_attribute ld |> Option.map (fun _ -> Uri);
    Attribute.get uuid_attribute ld |> Option.map (fun _ -> Uuid);
    Attribute.get dive_attribute ld |> Option.map (fun _ -> Dive);
  ]
  |> List.filter_map (fun x -> x)

let length_ident f =
  match f.field_type with
  | String ->
      Exp.(ident { txt = Ldot (Lident "String", "length"); loc = f.loc })
  | List _ -> Exp.(ident { txt = Ldot (Lident "List", "length"); loc = f.loc })
  | _ ->
      Location.raise_errorf ~loc:f.loc "length is not supported for this type"

let max_length_validator_ext max record_field =
  let open Exp in
  apply
    (ident
       {
         txt = Ldot (Lident "Validator", "validate_max_length");
         loc = record_field.loc;
       })
    [
      (Nolabel, length_ident record_field);
      (Nolabel, constant (Pconst_integer (string_of_int max, None)));
    ]

let validator_ext record_field = function
  | MaxLength max -> max_length_validator_ext max record_field
  | _ -> failwith "not implemented"
