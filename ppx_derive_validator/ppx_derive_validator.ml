open Ppxlib
open Ast_helper
open Validators
open Utils

let map_type_declaration ~loc td =
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
               { txt = Ldot (Lident "Validator", "record"); loc = td.ptype_loc })
            [ (Nolabel, expr_list td.ptype_loc field_validators) ])
      in

      let body =
        Exp.(
          apply
            (ident
               {
                 txt = Ldot (Lident "Validator", "validate");
                 loc = td.ptype_loc;
               })
            [ (Nolabel, body) ])
      in

      let record_name = td.ptype_name.txt in
      let function_name = "validate_" ^ record_name in

      let pattern = Pat.var { txt = function_name; loc } in
      let value_binding = Vb.mk pattern body in

      let function_item = Str.value Nonrecursive [ value_binding ] in
      [ function_item ]
  | _ -> Location.raise_errorf ~loc "Unsupported type"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations |> List.map (map_type_declaration ~loc) |> List.concat

let () =
  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "validator" ~str_type_decl:impl_generator |> Deriving.ignore
