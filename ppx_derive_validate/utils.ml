open Ppxlib
open Ast_helper

let rec expr_list loc = function
  | [] -> Exp.construct { txt = Lident "[]"; loc } None
  | x :: xs ->
      Exp.construct { txt = Lident "::"; loc }
        (Some (Exp.tuple [ x; expr_list loc xs ]))
