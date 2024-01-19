(* type variant = Record of { id : string } | Other of { d : string } *)
(* [@@deriving validate, eq, show] *)

(* let variant_testable = Alcotest.testable pp_variant equal_variant *)

(* let variant_ok () = *)
(*   let v = Record { id = "id" } in *)
(*   Alcotest.(check (result variant_testable Error.validation_error_testable)) *)
(*     "Ok" (Ok v) (validate_variant v) *)

(* let t = *)
(*   let open Alcotest in *)
(*   ("variant", [ test_case "variant - Ok" `Quick variant_ok ]) *)
