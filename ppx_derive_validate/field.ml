open Ppxlib

type record_field = { name : string; loc_type : loc_type }
and loc_type = { loc : Location.t; typ : simple_type }

and simple_type =
  | Bool
  | Int
  | Float
  | String
  | Option of (simple_type * core_type)
  | Other of longident
  | List of (simple_type * core_type)
  | Tuple of (simple_type * core_type) list

let rec extract_field_type label_loc (t : core_type) =
  let field_type =
    match t.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> String
    | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Int
    | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Bool
    | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Float
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]) ->
        Option (extract_field_type label_loc arg, arg)
    | Ptyp_constr ({ txt = Lident "list"; _ }, [ arg ]) ->
        List (extract_field_type label_loc arg, arg)
    | Ptyp_constr ({ txt = name; _ }, []) -> Other name
    | Ptyp_tuple inner_types ->
        Tuple
          (List.map
             (fun t ->
               let field_type = extract_field_type label_loc t in
               (field_type, t))
             inner_types)
    | _ -> Location.raise_errorf ~loc:label_loc "Unsupported type"
  in
  field_type

let extract_loc_type (t : core_type) =
  { loc = t.ptyp_loc; typ = extract_field_type t.ptyp_loc t }

let extract_record_field (ld : label_declaration) =
  let name = ld.pld_name.txt in
  let loc_type = extract_loc_type ld.pld_type in
  { name; loc_type }
