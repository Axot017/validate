open Ppxlib
open Ast_helper

type record_field = {
  name : string;
  field_type : record_field_type;
  validators : validators;
}
[@@deriving show]

and record_field_type =
  | String
  | Int
  | Bool
  | Float
  | Option of record_field_type
  | Custom of string
  | List of record_field_type
[@@deriving show]

and validators = {
  required : bool;
  email : bool;
  uri : bool;
  min_length : int option;
  max_length : int option;
  min_value : int option;
  max_value : int option;
  uuid : bool;
  dive : bool;
}
[@@deriving show]

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
  let validators =
    {
      min_length = Attribute.get min_length_attribute ld;
      max_length = Attribute.get max_length_attribute ld;
      min_value = Attribute.get min_value_attribute ld;
      max_value = Attribute.get max_value_attribute ld;
      required = Attribute.get required_attribute ld |> Option.is_some;
      email = Attribute.get email_attribute ld |> Option.is_some;
      uri = Attribute.get uri_attribute ld |> Option.is_some;
      uuid = Attribute.get uuid_attribute ld |> Option.is_some;
      dive = Attribute.get dive_attribute ld |> Option.is_some;
    }
  in

  (* Location.raise_errorf ~loc:ld.pld_name.loc "min_length: %s" *)
  (*   (show_validators validators) *)
  (* |> ignore; *)
  validators

let rec extract_field_type label_loc (t : core_type) =
  let field_type =
    match t.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> String
    | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Int
    | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Bool
    | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Float
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]) ->
        Option (extract_field_type label_loc arg)
    | Ptyp_constr ({ txt = Lident "list"; _ }, [ arg ]) ->
        List (extract_field_type label_loc arg)
    | Ptyp_constr ({ txt = Lident name; _ }, []) -> Custom name
    | _ -> Location.raise_errorf ~loc:label_loc "Unsupported type"
  in
  field_type

let extract_record_field (ld : label_declaration) =
  let field_type = extract_field_type ld.pld_name.loc ld.pld_type in
  let validators = extract_validators ld in
  let record_field = { name = ld.pld_name.txt; field_type; validators } in
  (* Location.raise_errorf ~loc:ld.pld_name.loc "field: %s" *)
  (*   (show_record_field record_field) *)
  (* |> ignore; *)
  record_field

let rec expr_list loc = function
  | [] -> Exp.construct { txt = Lident "[]"; loc } None
  | x :: xs ->
      Exp.construct { txt = Lident "::"; loc }
        (Some (Exp.tuple [ x; expr_list loc xs ]))

let generate_field_validations (f : record_field) =
  let open Exp in
  (* Function to create a validation expression for min_length *)
  let min_length_expr min_len =
    apply
      (ident
         {
           txt = Ldot (Lident "Validator", "validate_min_length");
           loc = !default_loc;
         })
      [
        ( Nolabel,
          ident { txt = Ldot (Lident "String", "length"); loc = !default_loc }
        );
        (Nolabel, constant (Pconst_integer (string_of_int min_len, None)));
      ]
  in

  (* Function to create a validation expression for max_length *)
  let max_length_expr max_len =
    apply
      (ident
         {
           txt = Ldot (Lident "Validator", "validate_max_length");
           loc = !default_loc;
         })
      [
        ( Nolabel,
          ident { txt = Ldot (Lident "String", "length"); loc = !default_loc }
        );
        (Nolabel, constant (Pconst_integer (string_of_int max_len, None)));
      ]
  in

  (* Match validators and generate corresponding expressions *)
  let validations =
    match f.validators with
    | { min_length = Some min_len; max_length = Some max_len; _ } ->
        [ min_length_expr min_len; max_length_expr max_len ]
    | { min_length = Some min_len; _ } -> [ min_length_expr min_len ]
    | { max_length = Some max_len; _ } -> [ max_length_expr max_len ]
    | _ -> []
  in

  let field_access_lambda =
    fun_ Nolabel None
      (Pat.var { txt = "x"; loc = !default_loc })
      (field
         (ident { txt = Lident "x"; loc = !default_loc })
         { txt = Lident f.name; loc = !default_loc })
  in

  apply
    (ident { txt = Ldot (Lident "Validator", "field"); loc = !default_loc })
    [
      (Nolabel, constant (Pconst_string (f.name, !default_loc, None)));
      (Nolabel, field_access_lambda);
      (Nolabel, expr_list !default_loc validations);
    ]

let map_type_declaration ~loc td =
  match td.ptype_kind with
  | Ptype_record label_declarations ->
      let field_validators =
        label_declarations
        |> List.map extract_record_field
        |> List.map generate_field_validations
      in

      (* |> List.map Pprintast.string_of_expression *)
      (* |> List.iter (Printf.printf "%s\n") *)
      let body =
        Exp.(
          apply
            (ident
               { txt = Ldot (Lident "Validator", "record"); loc = !default_loc })
            [ (Nolabel, expr_list !default_loc field_validators) ])
      in

      let body =
        Exp.(
          apply
            (ident
               {
                 txt = Ldot (Lident "Validator", "validate");
                 loc = !default_loc;
               })
            [ (Nolabel, body) ])
      in

      (* Create the function name *)
      let record_name = td.ptype_name.txt in
      let function_name = "validate_" ^ record_name in

      (* Construct the value binding for the function *)
      let pattern = Pat.var { txt = function_name; loc } in
      let value_binding = Vb.mk pattern body in

      (* Create the structure item *)
      let function_item = Str.value Nonrecursive [ value_binding ] in
      [ function_item ]
  | _ -> []

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations |> List.map (map_type_declaration ~loc) |> List.concat

let () =
  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "validator" ~str_type_decl:impl_generator |> Deriving.ignore
