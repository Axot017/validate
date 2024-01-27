open Err

let test_validate_uuid () =
  let uuid = "f47ac10b-58cc-4372-a567-0e02b2c3d479" in
  let result = Validate.validate_uuid uuid in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_uuid_invalid () =
  let uuid = "f47ac10b-58cc-4372-a567-0e02b2c3d4791" in
  let result = Validate.validate_uuid uuid in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          { code = Validate.invalid_uuid_error_code; params = [] }))
    result

let uuid =
  let open Alcotest in
  ( "validate_uuid",
    [
      test_case "Valid uuid" `Quick test_validate_uuid;
      test_case "Invalid uuid" `Quick test_validate_uuid_invalid;
    ] )

let test_validate_ulid () =
  let ulid = "01D3XZ1ZQZQZQZQZQZQZQZQZQZ" in
  let result = Validate.validate_ulid ulid in

  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_ulid_invalid () =
  let ulid = "01D3XZ1ZQZQZQZQZQZQZQZQZQZ1" in
  let result = Validate.validate_ulid ulid in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          { code = Validate.invalid_ulid_error_code; params = [] }))
    result

let ulid =
  let open Alcotest in
  ( "validate_ulid",
    [
      test_case "Valid ulid" `Quick test_validate_ulid;
      test_case "Invalid ulid" `Quick test_validate_ulid_invalid;
    ] )

let t = [ uuid; ulid ]
