open Ppxlib
open Ast_helper

let var_pat ~loc name = Pat.var { txt = name; loc }

let n_element_tuple_pat ~prefix ~loc n =
  match n with
  | 0 -> Pat.construct { txt = Lident "()"; loc } None
  | 1 -> var_pat ~loc (Printf.sprintf "%s0" prefix)
  | _ ->
      Pat.tuple
        (List.init n (fun i -> var_pat ~loc (Printf.sprintf "%s%d" prefix i)))

let record_pat ~loc fields =
  let fields =
    List.map
      (fun name -> ({ txt = Lident name; loc }, Pat.var { txt = name; loc }))
      fields
  in
  Pat.record fields Closed
