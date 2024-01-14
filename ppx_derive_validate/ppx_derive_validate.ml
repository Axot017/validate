open Ppxlib
open Ast_helper
open Validators
open Utils

let map_type_declaration ~loc td =
  let body =
    match td.ptype_kind with
    | Ptype_record label_declarations ->
        let field_validators =
          label_declarations |> List.map field_validator_exp
        in

        (* field_validators *)
        (* |> List.map Pprintast.string_of_expression *)
        (* |> List.iter (Printf.printf "%s\n"); *)
        let body =
          Exp.(
            apply
              (ident
                 {
                   txt = Ldot (Lident "Validate", "record");
                   loc = td.ptype_loc;
                 })
              [ (Nolabel, expr_list td.ptype_loc field_validators) ])
        in

        Exp.(
          apply
            (ident
               {
                 txt = Ldot (Lident "Validate", "validate");
                 loc = td.ptype_loc;
               })
            [ (Nolabel, body) ])
    | Ptype_abstract ->
        let validators =
          td.ptype_manifest |> Option.get |> type_validator_exp
        in
        (* Printf.printf "%s\n" (Pprintast.string_of_expression validators); *)
        Exp.(
          apply
            (ident
               {
                 txt = Ldot (Lident "Validate", "validate");
                 loc = td.ptype_loc;
               })
            [ (Nolabel, validators) ])
    | _ -> Location.raise_errorf ~loc "Unsupported type"
  in
  let type_name = td.ptype_name.txt in
  let function_name = "validate_" ^ type_name in

  let pattern = Pat.var { txt = function_name; loc } in
  let value_binding = Vb.mk pattern body in

  let function_item = Str.value Nonrecursive [ value_binding ] in
  function_item

let map_sig ~loc td =
  match td.ptype_kind with
  | Ptype_abstract | Ptype_record _ ->
      let record_name = td.ptype_name.txt in
      let function_name = "validate_" ^ record_name in
      let function_name_loc = { txt = function_name; loc } in

      let input_type = Typ.constr { txt = Lident record_name; loc } [] in
      let output_type =
        Typ.constr
          { txt = Ldot (Lident "Validate", "validation_error"); loc }
          []
      in
      let result_type =
        Typ.constr { txt = Lident "result"; loc } [ input_type; output_type ]
      in
      let function_type = Typ.arrow Nolabel input_type result_type in
      Sig.value (Val.mk function_name_loc function_type)
  | _ -> Location.raise_errorf ~loc "Unsupported type"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations |> List.map (map_type_declaration ~loc)

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations |> List.map (map_sig ~loc)

let () =
  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl in
  let intf_generator = Deriving.Generator.V2.make_noarg generate_intf in
  Deriving.add "validate" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
  |> Deriving.ignore
