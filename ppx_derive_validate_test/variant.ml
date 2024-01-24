type tuple_variant =
  | EmailToId of (string[@email]) * (int[@greater_than_or_equal 0])
  | Email of (string[@email])
[@@deriving validate, eq, show]

let tuple_variant_testable =
  Alcotest.testable pp_tuple_variant equal_tuple_variant

let tuple_variant_email_to_id_ok () =
  let v = EmailToId ("test@gmail.com", 1) in
  Alcotest.(
    check (result tuple_variant_testable Error.validation_error_testable))
    "Ok" (Ok v) (validate_tuple_variant v)

let tuple_variant_email_ok () =
  let v = Email "test@gmail.com" in
  Alcotest.(
    check (result tuple_variant_testable Error.validation_error_testable))
    "Ok" (Ok v) (validate_tuple_variant v)

let tuple_variant_email_to_id_error () =
  let v = EmailToId ("invalid", -1) in
  Alcotest.(
    check (result tuple_variant_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "EmailToId.1",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "0") ];
                  };
              ] );
            ( "EmailToId.0",
              [ Validate.BaseError { code = "invalid_email"; params = [] } ] );
          ]))
    (validate_tuple_variant v)

let tuple_variant_email_error () =
  let v = Email "invalid" in
  Alcotest.(
    check (result tuple_variant_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "Email.0",
              [ Validate.BaseError { code = "invalid_email"; params = [] } ] );
          ]))
    (validate_tuple_variant v)

type record_variant =
  | ID of { id : int [@greater_than_or_equal 0] }
  | URL of { url : (string[@url]) }
[@@deriving validate, eq, show]

let record_variant_testable =
  Alcotest.testable pp_record_variant equal_record_variant

let record_variant_id_ok () =
  let v = ID { id = 1 } in
  Alcotest.(
    check (result record_variant_testable Error.validation_error_testable))
    "Ok" (Ok v)
    (validate_record_variant v)

let record_variant_url_ok () =
  let v = URL { url = "https://www.google.com" } in
  Alcotest.(
    check (result record_variant_testable Error.validation_error_testable))
    "Ok" (Ok v)
    (validate_record_variant v)

let record_variant_id_error () =
  let v = ID { id = -2137 } in
  Alcotest.(
    check (result record_variant_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "ID.id",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "0") ];
                  };
              ] );
          ]))
    (validate_record_variant v)

let record_variant_url_error () =
  let v = URL { url = "invalid" } in
  Alcotest.(
    check (result record_variant_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "URL.url",
              [ Validate.BaseError { code = "invalid_url"; params = [] } ] );
          ]))
    (validate_record_variant v)

let t =
  let open Alcotest in
  ( "variant",
    [
      test_case "tuple_variant.EmailToId - Ok" `Quick
        tuple_variant_email_to_id_ok;
      test_case "tuple_variant.Email  - Ok" `Quick tuple_variant_email_ok;
      test_case "tuple_variant.EmailToId - Error" `Quick
        tuple_variant_email_to_id_error;
      test_case "tuple_variant.Email - Error" `Quick tuple_variant_email_error;
      test_case "record_variant.ID - Ok" `Quick record_variant_id_ok;
      test_case "record_variant.URL - Ok" `Quick record_variant_url_ok;
      test_case "record_variant.ID - Error" `Quick record_variant_id_error;
      test_case "record_variant.URL - Error" `Quick record_variant_url_error;
    ] )
