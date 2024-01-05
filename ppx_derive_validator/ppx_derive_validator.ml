open Ppxlib
open Ast_helper
open Validators
open Field
open Utils

let generate_field_validations (f, validators) =
  (* Match validators and generate corresponding expressions *)
  let open Exp in
  let field_validator_ext = validator_ext f in
  let exps = validators |> List.map field_validator_ext in

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
      (Nolabel, expr_list !default_loc exps);
    ]

let extract_field_data (f : label_declaration) =
  let validators = extract_validators f in
  let record_field = extract_record_field f in

  (record_field, validators)

let map_type_declaration ~loc td =
  match td.ptype_kind with
  | Ptype_record label_declarations ->
      let field_validators =
        label_declarations
        |> List.map extract_field_data
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
