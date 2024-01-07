open Ppxlib

type record_field = {
  name : string;
  field_type : record_field_type;
  loc : Location.t;
}

and record_field_type =
  | Bool
  | Int
  | Float
  | String
  | Option of record_field_type
  | Other of longident
  | List of record_field_type

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
    | Ptyp_constr ({ txt = name; _ }, []) -> Other name
    | _ -> Location.raise_errorf ~loc:label_loc "Unsupported type"
  in
  field_type

let extract_record_field (ld : label_declaration) =
  let field_type = extract_field_type ld.pld_name.loc ld.pld_type in
  let record_field = { name = ld.pld_name.txt; field_type; loc = ld.pld_loc } in
  record_field
