open Err

let test_validate_url_error () =
  let result = Validator.validate_url "invalid_url" in

  Alcotest.(check (result unit validation_error_testable))
    "validate_url"
    (Error (Validator.BaseError { code = "invalid_url"; params = [] }))
    result

let test_validate_url_success () =
  let result = Validator.validate_url "https://www.google.com" in

  Alcotest.(check (result unit validation_error_testable))
    "validate_url" (Ok ()) result

let validate_url =
  let open Alcotest in
  ( "validate_url",
    [
      test_case "Error - invalid url" `Quick test_validate_url_error;
      test_case "Success" `Quick test_validate_url_success;
    ] )

let t = [ validate_url ]
