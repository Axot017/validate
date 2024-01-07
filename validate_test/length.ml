open Err

let test_validate_string_min_length_long () =
  let min_length = 5 in
  let string = "123456" in
  let result = Validate.validate_min_length String.length min_length string in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_list_min_length_long () =
  let min_length = 5 in
  let list = [ 1; 2; 3; 4; 5 ] in
  let result = Validate.validate_min_length List.length min_length list in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_string_min_length_short () =
  let min_length = 5 in
  let string = "1234" in
  let result = Validate.validate_min_length String.length min_length string in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          {
            code = Validate.min_length_error_code;
            params = [ ("threshold", string_of_int min_length) ];
          }))
    result

let test_validate_list_min_length_short () =
  let min_length = 5 in
  let list = [ 1; 2; 3; 4 ] in
  let result = Validate.validate_min_length List.length min_length list in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          {
            code = Validate.min_length_error_code;
            params = [ ("threshold", string_of_int min_length) ];
          }))
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
  let result = Validate.validate_max_length String.length max_length string in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          {
            code = Validate.max_length_error_code;
            params = [ ("threshold", string_of_int max_length) ];
          }))
    result

let test_validate_string_max_length_short () =
  let max_length = 5 in
  let string = "1234" in
  let result = Validate.validate_max_length String.length max_length string in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_list_max_length_zero () =
  let max_length = 0 in
  let list = [] in
  let result = Validate.validate_max_length List.length max_length list in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let max_string_length =
  let open Alcotest in
  ( "validate_max_length",
    [
      test_case "Long enough string" `Quick test_validate_string_max_length_long;
      test_case "Too short string" `Quick test_validate_string_max_length_short;
      test_case "Zero length list" `Quick test_validate_list_max_length_zero;
    ] )

let test_validate_string_length_between_long () =
  let min = 5 in
  let max = 7 in
  let string = "1234567890" in
  let result =
    Validate.validate_length_between String.length ~min ~max string
  in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          {
            code = Validate.max_length_error_code;
            params = [ ("threshold", string_of_int max) ];
          }))
    result

let test_validate_string_length_between_short () =
  let min = 5 in
  let max = 7 in
  let string = "1234" in
  let result =
    Validate.validate_length_between String.length ~min ~max string
  in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          {
            code = Validate.min_length_error_code;
            params = [ ("threshold", string_of_int min) ];
          }))
    result

let test_validate_string_length_between_ok () =
  let min = 5 in
  let max = 7 in
  let string = "123456" in
  let result =
    Validate.validate_length_between String.length ~min ~max string
  in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let length_between =
  let open Alcotest in
  ( "validate_length_between",
    [
      test_case "String too long" `Quick
        test_validate_string_length_between_long;
      test_case "String too short" `Quick
        test_validate_string_length_between_short;
      test_case "String ok" `Quick test_validate_string_length_between_ok;
    ] )

let test_validate_string_length_equals_long () =
  let length = 5 in
  let string = "123456" in
  let result = Validate.validate_length_equals String.length length string in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          {
            code = Validate.length_equals_error_code;
            params = [ ("value", string_of_int length) ];
          }))
    result

let test_validate_string_length_equals_ok () =
  let length = 5 in
  let string = "12345" in
  let result = Validate.validate_length_equals String.length length string in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let lenght_equals_string =
  let open Alcotest in
  ( "validate_length_equals",
    [
      test_case "String too long" `Quick test_validate_string_length_equals_long;
      test_case "String ok" `Quick test_validate_string_length_equals_ok;
    ] )

let t =
  [ min_string_length; max_string_length; length_between; lenght_equals_string ]
