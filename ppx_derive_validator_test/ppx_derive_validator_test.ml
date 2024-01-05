type test_record = { min : string; max : string [@max_length 5] }
[@@deriving validator, show, eq]

let test_record_testable = Alcotest.testable pp_test_record equal_test_record

let validation_error_testable =
  Alcotest.testable Validator.pp_validation_error
    Validator.equal_validation_error

let test_length_generation_error () =
  let result = validate_test_record { min = "1"; max = "123456" } in
  Alcotest.(check (result test_record_testable validation_error_testable))
    "returns Error"
    (Error
       (Validator.RecordError
          [
            ( "max",
              [
                Validator.BaseError
                  { code = "max_length"; params = [ ("threshold", "5") ] };
              ] );
            ( "min",
              [
                Validator.BaseError
                  { code = "min_length"; params = [ ("threshold", "2") ] };
              ] );
          ]))
    result

let test_length_generation_ok () =
  let r = { min = "12"; max = "12345" } in
  let result = validate_test_record r in
  Alcotest.(check (result test_record_testable validation_error_testable))
    "returns Ok" (Ok r) result

let () =
  let open Alcotest in
  run "Validator"
    [
      ( "deriving",
        [
          test_case "String length - error" `Quick test_length_generation_error;
          test_case "String length - ok" `Quick test_length_generation_ok;
        ] );
    ]
