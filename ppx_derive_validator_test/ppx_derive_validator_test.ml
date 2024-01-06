type test_record = {
  min : string; [@min_length 2]
  max : string; [@max_length 5]
  url : string; [@url]
  uuid : string; [@uuid]
  numeric : string; [@numeric]
  alpha : string; [@alpha]
  alphanumeric : string; [@alphanumeric]
  lowercase : string; [@lowercase]
  lowercase_alphanumeric : string; [@lowercase_alphanumeric]
  uppercase : string; [@uppercase]
  uppercase_alphanumeric : string; [@uppercase_alphanumeric]
}
[@@deriving validator, show, eq]

let test_record_testable = Alcotest.testable pp_test_record equal_test_record

let validation_error_testable =
  Alcotest.testable Validator.pp_validation_error
    Validator.equal_validation_error

let test_err () =
  let result =
    validate_test_record
      {
        min = "1";
        max = "123456";
        url = "invalid url";
        uuid = "invalid uuid";
        numeric = "123a";
        alpha = "123";
        alphanumeric = "122@";
        lowercase = "aBC";
        lowercase_alphanumeric = "aBC@";
        uppercase = "Abc";
        uppercase_alphanumeric = "Abc@";
      }
  in
  Alcotest.(check (result test_record_testable validation_error_testable))
    "returns Error"
    (Error
       (Validator.RecordError
          [
            ( "uppercase_alphanumeric",
              [
                Validator.BaseError
                  { code = "invalid_uppercase_alphanumeric"; params = [] };
              ] );
            ( "uppercase",
              [
                Validator.BaseError { code = "invalid_uppercase"; params = [] };
              ] );
            ( "lowercase_alphanumeric",
              [
                Validator.BaseError
                  { code = "invalid_lowercase_alphanumeric"; params = [] };
              ] );
            ( "lowercase",
              [
                Validator.BaseError { code = "invalid_lowercase"; params = [] };
              ] );
            ( "alphanumeric",
              [
                Validator.BaseError
                  { code = "invalid_alphanumeric"; params = [] };
              ] );
            ( "alpha",
              [ Validator.BaseError { code = "invalid_alpha"; params = [] } ] );
            ( "numeric",
              [ Validator.BaseError { code = "invalid_numeric"; params = [] } ]
            );
            ( "uuid",
              [ Validator.BaseError { code = "invalid_uuid"; params = [] } ] );
            ( "url",
              [ Validator.BaseError { code = "invalid_url"; params = [] } ] );
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

let test_ok () =
  let r =
    {
      min = "12";
      max = "12345";
      url = "https://www.google.com";
      uuid = "123e4567-e89b-12d3-a456-426614174000";
      numeric = "123";
      alpha = "abc";
      alphanumeric = "123abc";
      lowercase = "abc";
      lowercase_alphanumeric = "abc123";
      uppercase = "ABC";
      uppercase_alphanumeric = "ABC123";
    }
  in
  let result = validate_test_record r in
  Alcotest.(check (result test_record_testable validation_error_testable))
    "returns Ok" (Ok r) result

let () =
  let open Alcotest in
  run "Validator"
    [
      ( "deriving",
        [ test_case "Error" `Quick test_err; test_case "Ok" `Quick test_ok ] );
    ]
