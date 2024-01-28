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

let test_validate_ipv4 () =
  let ipv4 = "192.168.0.1" in
  let result = Validate.validate_ipv4 ipv4 in
  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_ipv4_invalid () =
  let ipv4 = "192.168.0.256" in
  let result = Validate.validate_ipv4 ipv4 in

  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          { code = Validate.invalid_ipv4_error_code; params = [] }))
    result

let ipv4 =
  let open Alcotest in
  ( "validate_ipv4",
    [
      test_case "Valid ipv4" `Quick test_validate_ipv4;
      test_case "Invalid ipv4" `Quick test_validate_ipv4_invalid;
    ] )

let test_validate_ipv6 () =
  let ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334" in
  let result = Validate.validate_ipv6 ipv6 in
  Alcotest.(check (result unit validation_error_testable))
    "returns Ok" (Ok ()) result

let test_validate_ipv6_invalid () =
  let ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334:1" in
  let result = Validate.validate_ipv6 ipv6 in
  Alcotest.(check (result unit validation_error_testable))
    "returns Error"
    (Error
       (Validate.BaseError
          { code = Validate.invalid_ipv6_error_code; params = [] }))
    result

let ipv6 =
  let open Alcotest in
  ( "validate_ipv6",
    [
      test_case "Valid ipv6" `Quick test_validate_ipv6;
      test_case "Invalid ipv6" `Quick test_validate_ipv6_invalid;
    ] )

let t = [ uuid; ulid; ipv4; ipv6 ]
