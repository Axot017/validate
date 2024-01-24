open Ppxlib
open Ast_helper
open Validators

let map_type_declaration ~loc td =
  let body =
    match td.ptype_kind with
    | Ptype_record label_declarations ->
        validate_record_exp ~loc label_declarations
    | Ptype_abstract ->
        td.ptype_manifest |> Option.get |> validate_abstract_exp ~loc
    | Ptype_variant constructor_declarations ->
        validate_variant_exp ~loc constructor_declarations
    | _ -> Location.raise_errorf ~loc "Unsupported type"
  in
  let type_name = td.ptype_name.txt in

  let param_pattern = Pat.var { txt = "x"; loc } in
  let param_type = Typ.constr { txt = Lident type_name; loc } [] in
  let typed_param_pattern = Pat.constraint_ param_pattern param_type in
  let func_expr = Exp.fun_ Nolabel None typed_param_pattern body in

  let function_name = "validate_" ^ type_name in

  let function_pattern = Pat.var { txt = function_name; loc } in
  Str.value Nonrecursive [ Vb.mk function_pattern func_expr ]

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
