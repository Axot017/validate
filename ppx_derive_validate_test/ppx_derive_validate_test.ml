module Test : sig
  type t = { min_module : string [@min_length 2] }
  [@@deriving validate, show, eq]
end = struct
  type t = { min_module : string [@min_length 2] }
  [@@deriving validate, show, eq]
end

let _ = Test.show

type other_test_record = { other_min : string [@min_length 2] }
[@@deriving validate, show, eq]

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
  less_than : int; [@less_than 10]
  less_than_or_equal : int; [@less_than_or_equal 10]
  greater_than : int; [@greater_than 5]
  greater_than_or_equal : int; [@greater_than_or_equal 5]
  equal_to : int; [@equal_to 5]
  not_equal_to : int; [@not_equal_to 10]
  option_some : string option; [@min_length 2]
  option_none : string option; [@min_length 2]
  test_list : string list; [@min_length 2]
  list_min_length : string list; [@list_min_length 3] [@min_length 1]
  list_max_length : string list; [@list_max_length 5] [@max_length 3]
  other_test_record : other_test_record; [@dive]
  module_test_record : Test.t; [@dive]
  other_test_record_list : other_test_record list; [@dive] [@list_min_length 2]
}
[@@deriving validate, show, eq]

(* type aaa = (string list list[@min_length 2]) [@@deriving validator] *)

let test_record_testable = Alcotest.testable pp_test_record equal_test_record

let validation_error_testable =
  Alcotest.testable Validate.pp_validation_error Validate.equal_validation_error

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
        less_than = 10;
        less_than_or_equal = 11;
        greater_than = 5;
        greater_than_or_equal = 4;
        equal_to = 4;
        not_equal_to = 10;
        option_some = Some "1";
        option_none = None;
        test_list = [ "1"; "234"; "5" ];
        list_min_length = [ ""; "2" ];
        list_max_length = [ "1"; "2"; "3"; "4"; "5"; "67890" ];
        other_test_record = { other_min = "1" };
        module_test_record = { Test.min_module = "1" };
        other_test_record_list = [ { other_min = "1" } ];
      }
  in
  Alcotest.(check (result test_record_testable validation_error_testable))
    "returns Error"
    (Error
       (Validate.RecordError
          [
            ( "other_test_record_list",
              [
                Validate.IterableError
                  [
                    ( 0,
                      [
                        Validate.RecordError
                          [
                            ( "other_min",
                              [
                                Validate.BaseError
                                  {
                                    code = "min_length";
                                    params = [ ("threshold", "2") ];
                                  };
                              ] );
                          ];
                      ] );
                  ];
                Validate.BaseError
                  { code = "min_length"; params = [ ("threshold", "2") ] };
              ] );
            ( "module_test_record",
              [
                Validate.RecordError
                  [
                    ( "min_module",
                      [
                        Validate.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "other_test_record",
              [
                Validate.RecordError
                  [
                    ( "other_min",
                      [
                        Validate.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "list_max_length",
              [
                Validate.IterableError
                  [
                    ( 5,
                      [
                        Validate.BaseError
                          {
                            Validate.code = "max_length";
                            params = [ ("threshold", "3") ];
                          };
                      ] );
                  ];
                Validate.BaseError
                  { code = "max_length"; params = [ ("threshold", "5") ] };
              ] );
            ( "list_min_length",
              [
                Validate.IterableError
                  [
                    ( 0,
                      [
                        Validate.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "1") ];
                          };
                      ] );
                  ];
                Validate.BaseError
                  { code = "min_length"; params = [ ("threshold", "3") ] };
              ] );
            ( "test_list",
              [
                Validate.IterableError
                  [
                    ( 2,
                      [
                        Validate.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                    ( 0,
                      [
                        Validate.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "option_some",
              [
                Validate.BaseError
                  { code = "min_length"; params = [ ("threshold", "2") ] };
              ] );
            ( "not_equal_to",
              [
                Validate.BaseError
                  { code = "value_not_equal_to"; params = [ ("value", "10") ] };
              ] );
            ( "equal_to",
              [
                Validate.BaseError
                  { code = "value_equal_to"; params = [ ("value", "5") ] };
              ] );
            ( "greater_than_or_equal",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "5") ];
                  };
              ] );
            ( "greater_than",
              [
                Validate.BaseError
                  {
                    code = "value_greater_than";
                    params = [ ("threshold", "5") ];
                  };
              ] );
            ( "less_than_or_equal",
              [
                Validate.BaseError
                  {
                    code = "value_less_than_or_equal";
                    params = [ ("threshold", "10") ];
                  };
              ] );
            ( "less_than",
              [
                Validate.BaseError
                  { code = "value_less_than"; params = [ ("threshold", "10") ] };
              ] );
            ( "uppercase_alphanumeric",
              [
                Validate.BaseError
                  { code = "invalid_uppercase_alphanumeric"; params = [] };
              ] );
            ( "uppercase",
              [ Validate.BaseError { code = "invalid_uppercase"; params = [] } ]
            );
            ( "lowercase_alphanumeric",
              [
                Validate.BaseError
                  { code = "invalid_lowercase_alphanumeric"; params = [] };
              ] );
            ( "lowercase",
              [ Validate.BaseError { code = "invalid_lowercase"; params = [] } ]
            );
            ( "alphanumeric",
              [
                Validate.BaseError
                  { code = "invalid_alphanumeric"; params = [] };
              ] );
            ( "alpha",
              [ Validate.BaseError { code = "invalid_alpha"; params = [] } ] );
            ( "numeric",
              [ Validate.BaseError { code = "invalid_numeric"; params = [] } ]
            );
            ( "uuid",
              [ Validate.BaseError { code = "invalid_uuid"; params = [] } ] );
            ("url", [ Validate.BaseError { code = "invalid_url"; params = [] } ]);
            ( "max",
              [
                Validate.BaseError
                  { code = "max_length"; params = [ ("threshold", "5") ] };
              ] );
            ( "min",
              [
                Validate.BaseError
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
      less_than = 9;
      less_than_or_equal = 10;
      greater_than = 6;
      greater_than_or_equal = 5;
      equal_to = 5;
      not_equal_to = 9;
      option_some = Some "12";
      option_none = None;
      test_list = [ "123"; "456"; "789" ];
      list_min_length = [ "1"; "2"; "3" ];
      list_max_length = [ "1"; "2"; "3"; "4"; "5" ];
      other_test_record = { other_min = "12" };
      module_test_record = { Test.min_module = "12" };
      other_test_record_list = [ { other_min = "12" }; { other_min = "12" } ];
    }
  in
  let result = validate_test_record r in
  Alcotest.(check (result test_record_testable validation_error_testable))
    "returns Ok" (Ok r) result

let () =
  let open Alcotest in
  run "Validate"
    [
      ( "deriving",
        [ test_case "Error" `Quick test_err; test_case "Ok" `Quick test_ok ] );
    ]
