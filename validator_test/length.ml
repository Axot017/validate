open Err

let test_validate_string_min_length_long () =
  let min_length = 5 in
  let string = "123456" in
  let result = Validator.validate_min_length String.length min_length string in

  Alcotest.(check (result string base_validation_error_testable))
    "returns Ok" (Ok string) result

let test_validate_list_min_length_long () =
  let min_length = 5 in
  let list = [ 1; 2; 3; 4; 5 ] in
  let result = Validator.validate_min_length List.length min_length list in

  Alcotest.(check (result (list int) base_validation_error_testable))
    "returns Ok" (Ok list) result

let test_validate_string_min_length_short () =
  let min_length = 5 in
  let string = "1234" in
  let result = Validator.validate_min_length String.length min_length string in

  Alcotest.(check (result string base_validation_error_testable))
    "returns Error"
    (Error
       {
         code = Validator.min_length_error_code;
         params = [ ("threshold", string_of_int min_length) ];
       })
    result

let test_validate_list_min_length_short () =
  let min_length = 5 in
  let list = [ 1; 2; 3; 4 ] in
  let result = Validator.validate_min_length List.length min_length list in

  Alcotest.(check (result (list int) base_validation_error_testable))
    "returns Error"
    (Error
       {
         code = Validator.min_length_error_code;
         params = [ ("threshold", string_of_int min_length) ];
       })
    result

let min_string_length =
  let open Alcotest in
  ( "validate_min_length",
    [
      test_case "Long enough string" `Quick test_validate_string_min_length_long;
      test_case "Too short string" `Quick test_validate_string_min_length_short;
      test_case "Long enough list" `Quick test_validate_list_min_length_long;
      test_case "Too short list" `Quick test_validate_list_min_length_short;
    ] )

let test_validate_string_max_length_long () =
  let max_length = 5 in
  let string = "123456" in
  let result = Validator.validate_max_length String.length max_length string in

  Alcotest.(check (result string base_validation_error_testable))
    "returns Error"
    (Error
       {
         code = Validator.max_length_error_code;
         params = [ ("threshold", string_of_int max_length) ];
       })
    result

let test_validate_string_max_length_short () =
  let max_length = 5 in
  let string = "1234" in
  let result = Validator.validate_max_length String.length max_length string in

  Alcotest.(check (result string base_validation_error_testable))
    "returns Ok" (Ok string) result

let test_validate_list_max_length_zero () =
  let max_length = 0 in
  let list = [] in
  let result = Validator.validate_max_length List.length max_length list in

  Alcotest.(check (result (list int) base_validation_error_testable))
    "returns Ok" (Ok list) result

let max_string_length =
  let open Alcotest in
  ( "validate_max_length",
    [
      test_case "Long enough string" `Quick test_validate_string_max_length_long;
      test_case "Too short string" `Quick test_validate_string_max_length_short;
      test_case "Zero length list" `Quick test_validate_list_max_length_zero;
    ] )

let t = [ min_string_length; max_string_length ]
