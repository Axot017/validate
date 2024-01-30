type cond_record = {
  unit : string;
  temperature : int;
      [@greater_than_or_equal 0] [@ignore_if fun r -> r.unit <> "F"]
}
[@@deriving validate, eq, show]

let cond_record_testable = Alcotest.testable pp_cond_record equal_cond_record

let test_cond_record_f_ok () =
  let r = { unit = "F"; temperature = 0 } in
  let result = validate_cond_record r in

  Alcotest.(check (result cond_record_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_cond_record_f_err () =
  let r = { unit = "F"; temperature = -10 } in
  let result = validate_cond_record r in

  Alcotest.(check (result cond_record_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "temperature",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "0") ];
                  };
              ] );
          ]))
    result

let test_cond_record_c_ok () =
  let r = { unit = "C"; temperature = -10 } in
  let result = validate_cond_record r in

  Alcotest.(check (result cond_record_testable Error.validation_error_testable))
    "Ok" (Ok r) result

type cond_tuple =
  string * (int[@greater_than_or_equal 0] [@ignore_if fun (u, _) -> u <> "F"])
[@@deriving validate, eq, show]

let cond_tuple_testable = Alcotest.testable pp_cond_tuple equal_cond_tuple

let test_cond_tuple_f_ok () =
  let r = ("F", 0) in
  let result = validate_cond_tuple r in

  Alcotest.(check (result cond_tuple_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_cond_tuple_f_err () =
  let r = ("F", -10) in
  let result = validate_cond_tuple r in

  Alcotest.(check (result cond_tuple_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.GroupError
          [
            Validate.KeyedError
              [
                ( "1",
                  [
                    Validate.BaseError
                      {
                        code = "value_greater_than_or_equal";
                        params = [ ("threshold", "0") ];
                      };
                  ] );
              ];
          ]))
    result

let test_cond_tuple_c_ok () =
  let r = ("C", -10) in
  let result = validate_cond_tuple r in

  Alcotest.(check (result cond_tuple_testable Error.validation_error_testable))
    "Ok" (Ok r) result

type cond_variant =
  | Tuple of
      string
      * (int
        [@greater_than_or_equal 0]
        [@ignore_if
          fun v -> match v with Tuple (u, _) -> u <> "F" | _ -> false])
  | Record of {
      unit : string;
      temperature : int;
          [@greater_than_or_equal 0]
          [@ignore_if
            fun v -> match v with Record r -> r.unit <> "F" | _ -> false]
    }
[@@deriving validate, eq, show]

let cond_variant_testable = Alcotest.testable pp_cond_variant equal_cond_variant

let test_cond_variant_tuple_f_ok () =
  let r = Tuple ("F", 0) in
  let result = validate_cond_variant r in

  Alcotest.(
    check (result cond_variant_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_cond_variant_tuple_f_err () =
  let r = Tuple ("F", -10) in
  let result = validate_cond_variant r in

  Alcotest.(
    check (result cond_variant_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "Tuple.1",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "0") ];
                  };
              ] );
          ]))
    result

let test_cond_variant_tuple_c_ok () =
  let r = Tuple ("C", -10) in
  let result = validate_cond_variant r in

  Alcotest.(
    check (result cond_variant_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_cond_variant_record_f_ok () =
  let r = Record { unit = "F"; temperature = 0 } in
  let result = validate_cond_variant r in

  Alcotest.(
    check (result cond_variant_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_cond_variant_record_f_err () =
  let r = Record { unit = "F"; temperature = -10 } in
  let result = validate_cond_variant r in

  Alcotest.(
    check (result cond_variant_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "Record.temperature",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "0") ];
                  };
              ] );
          ]))
    result

let test_cond_variant_record_c_ok () =
  let r = Record { unit = "C"; temperature = -10 } in
  let result = validate_cond_variant r in

  Alcotest.(
    check (result cond_variant_testable Error.validation_error_testable))
    "Ok" (Ok r) result

type username_or_email = {
  username : string option; [@some_if fun r -> r.email = None]
  email : string option; [@none_if fun r -> Option.is_some r.username]
}
[@@deriving validate, eq, show]

let username_or_email_testable =
  Alcotest.testable pp_username_or_email equal_username_or_email

let test_username_or_email_username_ok () =
  let r = { username = Some "username"; email = None } in
  let result = validate_username_or_email r in

  Alcotest.(
    check (result username_or_email_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_username_or_email_email_ok () =
  let r = { username = None; email = Some "email" } in
  let result = validate_username_or_email r in

  Alcotest.(
    check (result username_or_email_testable Error.validation_error_testable))
    "Ok" (Ok r) result

let test_username_or_email_both_err () =
  let r = { username = Some "username"; email = Some "email" } in
  let result = validate_username_or_email r in

  Alcotest.(
    check (result username_or_email_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "email",
              [ Validate.BaseError { code = "expect_none"; params = [] } ] );
            ( "username",
              [ Validate.BaseError { code = "expect_none"; params = [] } ] );
          ]))
    result

let test_username_or_email_both_none () =
  let r = { username = None; email = None } in
  let result = validate_username_or_email r in

  Alcotest.(
    check (result username_or_email_testable Error.validation_error_testable))
    "Error"
    (Error
       (Validate.KeyedError
          [
            ( "email",
              [ Validate.BaseError { code = "expect_some"; params = [] } ] );
            ( "username",
              [ Validate.BaseError { code = "expect_some"; params = [] } ] );
          ]))
    result

let t =
  let open Alcotest in
  ( "conditinal",
    [
      test_case "cond_record - F - Ok" `Quick test_cond_record_f_ok;
      test_case "cond_record - F - Error" `Quick test_cond_record_f_err;
      test_case "cond_record - C - Ok" `Quick test_cond_record_c_ok;
      test_case "cond_tuple - F - Ok" `Quick test_cond_tuple_f_ok;
      test_case "cond_tuple - F - Error" `Quick test_cond_tuple_f_err;
      test_case "cond_tuple - C - Ok" `Quick test_cond_tuple_c_ok;
      test_case "cond_variant - Tuple - F - Ok" `Quick
        test_cond_variant_tuple_f_ok;
      test_case "cond_variant - Tuple - F - Error" `Quick
        test_cond_variant_tuple_f_err;
      test_case "cond_variant - Tuple - C - Ok" `Quick
        test_cond_variant_tuple_c_ok;
      test_case "cond_variant - Record - F - Ok" `Quick
        test_cond_variant_record_f_ok;
      test_case "cond_variant - Record - F - Error" `Quick
        test_cond_variant_record_f_err;
      test_case "cond_variant - Record - C - Ok" `Quick
        test_cond_variant_record_c_ok;
      test_case "username_or_email - username - Ok" `Quick
        test_username_or_email_username_ok;
      test_case "username_or_email - email - Ok" `Quick
        test_username_or_email_email_ok;
      test_case "username_or_email - both - Error" `Quick
        test_username_or_email_both_err;
      test_case "username_or_email - both - None" `Quick
        test_username_or_email_both_none;
    ] )
