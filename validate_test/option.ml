let test_validate_some () =
  let value = Some "value" in
  let result = Validate.validate_some value in

  Alcotest.(check (result unit Err.validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_some_err () =
  let value = None in
  let result = Validate.validate_some value in

  Alcotest.(check (result unit Err.validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          { code = Validate.expect_some_error_code; params = [] }))
    result

let test_validate_none () =
  let value = None in
  let result = Validate.validate_none value in

  Alcotest.(check (result unit Err.validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_none_err () =
  let value = Some "value" in
  let result = Validate.validate_none value in

  Alcotest.(check (result unit Err.validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          { code = Validate.expect_none_error_code; params = [] }))
    result

let required =
  let open Alcotest in
  ( "required",
    [
      test_case "Some - Ok" `Quick test_validate_some;
      test_case "Some - Err" `Quick test_validate_some_err;
      test_case "None - Ok" `Quick test_validate_none;
      test_case "None - Err" `Quick test_validate_none_err;
    ] )

let t = [ required ]
