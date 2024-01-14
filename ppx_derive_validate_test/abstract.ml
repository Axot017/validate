type simple_string = (string[@email]) [@@deriving validate]

let test_simple_string_ok () =
  let email = "email@test.com" in
  let result = validate_simple_string email in
  Alcotest.(check (result string Error.validation_error_testable))
    "returns Ok" (Ok email) result

let test_simple_string_error () =
  let email = "" in
  let result = validate_simple_string email in
  Alcotest.(check (result string Error.validation_error_testable))
    "returns Error"
    (Error
       (Validate.GroupError
          [ Validate.BaseError { code = "invalid_email"; params = [] } ]))
    result

type list_type = (string list[@list_min_length 2] [@min_length 1])
[@@deriving validate]

let test_list_type_ok () =
  let list = [ "test"; "test" ] in
  let result = validate_list_type list in
  Alcotest.(check (result (list string) Error.validation_error_testable))
    "returns Ok" (Ok list) result

let test_list_type_error () =
  let list = [ "" ] in
  let result = validate_list_type list in
  Alcotest.(check (result (list string) Error.validation_error_testable))
    "returns Error"
    (Error
       (Validate.GroupError
          [
            Validate.IterableError
              [
                ( 0,
                  [
                    Validate.BaseError
                      { code = "min_length"; params = [ ("threshold", "1") ] };
                  ] );
              ];
            Validate.BaseError
              { code = "min_length"; params = [ ("threshold", "2") ] };
          ]))
    result

type optional_type = (string option[@min_length 1]) [@@deriving validate]

let test_optional_type_none () =
  let result = validate_optional_type None in
  Alcotest.(check (result (option string) Error.validation_error_testable))
    "returns Ok" (Ok None) result

let test_optional_type_some_ok () =
  let result = validate_optional_type (Some "test") in
  Alcotest.(check (result (option string) Error.validation_error_testable))
    "returns Ok" (Ok (Some "test")) result

let test_optional_type_some_error () =
  let result = validate_optional_type (Some "") in
  Alcotest.(check (result (option string) Error.validation_error_testable))
    "returns Error"
    (Error
       (Validate.GroupError
          [
            Validate.BaseError
              { code = "min_length"; params = [ ("threshold", "1") ] };
          ]))
    result

let t =
  let open Alcotest in
  ( "abstract type",
    [
      test_case "simple_string - Ok" `Quick test_simple_string_ok;
      test_case "simple_string - Error" `Quick test_simple_string_error;
      test_case "list_type - Ok" `Quick test_list_type_ok;
      test_case "list_type - Error" `Quick test_list_type_error;
      test_case "optional_type - None" `Quick test_optional_type_none;
      test_case "optional_type - Some Ok" `Quick test_optional_type_some_ok;
      test_case "optional_type - Some Error" `Quick
        test_optional_type_some_error;
    ] )
