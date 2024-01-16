type tuple = (string[@email]) * (int[@greater_than 1]) [@@deriving validate]

let test_tuple_ok () =
  let pair = ("email@test.com", 2) in
  let result = validate_tuple pair in
  Alcotest.(check (result (pair string int) Error.validation_error_testable))
    "returns Ok" (Ok pair) result

let test_tuple_error () =
  let pair = ("test", 0) in
  let result = validate_tuple pair in
  Alcotest.(check (result (pair string int) Error.validation_error_testable))
    "returns Ok"
    (Error
       (Validate.GroupError
          [
            Validate.RecordError
              [
                ( "1",
                  [
                    Validate.BaseError
                      {
                        code = "value_greater_than";
                        params = [ ("threshold", "1") ];
                      };
                  ] );
                ( "0",
                  [ Validate.BaseError { code = "invalid_email"; params = [] } ]
                );
              ];
          ]))
    result

let t =
  let open Alcotest in
  ( "tuple",
    [
      test_case "tuple - Ok" `Quick test_tuple_ok;
      test_case "tuple - Error" `Quick test_tuple_error;
    ] )
