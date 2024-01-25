type tree =
  | Leaf of (int[@greater_than 0])
  | Node of { left : tree; [@dive] right : (tree[@dive]) }
[@@deriving validate, show, eq]

let tree_testable = Alcotest.testable pp_tree equal_tree

let test_tree () =
  let tree =
    Node { left = Leaf 1; right = Node { left = Leaf 2; right = Leaf 3 } }
  in
  let result = validate_tree tree in
  Alcotest.(check (result tree_testable Error.validation_error_testable))
    "returns Ok" (Ok tree) result

type user = { id : int; [@greater_than 0] friends : (user[@dive]) list }
[@@deriving validate, show, eq]

let user_testable = Alcotest.testable pp_user equal_user

let test_user () =
  let user = { id = 1; friends = [ { id = 2; friends = [] } ] } in
  let result = validate_user user in
  Alcotest.(check (result user_testable Error.validation_error_testable))
    "returns Ok" (Ok user) result

(* type a = { a_id : int; [@greater_than 0] b : (b[@dive]) option } *)
(* [@@deriving validate, show, eq] *)
(**)
(* and b = { b_id : int; [@greater_than 0] a : (a[@dive]) option } *)
(* [@@deriving validate, show, eq] *)
(**)
(* let a_testable = Alcotest.testable pp_a equal_a *)
(* let b_testable = Alcotest.testable pp_b equal_b *)
(**)
(* let test_cycle () = *)
(*   let a = *)
(*     { a_id = 1; b = Some { b_id = 2; a = Some { a_id = 3; b = None } } } *)
(*   in *)
(*   let result = validate_a a in *)
(*   Alcotest.(check (result a_testable Error.validation_error_testable)) *)
(*     "returns Ok" (Ok a) result *)

let t =
  let open Alcotest in
  ( "recursive",
    [
      test_case "tree" `Quick test_tree;
      test_case "user" `Quick test_user;
      (* test_case "cycle" `Quick test_cycle; *)
    ] )
