module Test : sig
  type t = { min_module : string [@min_length 2] }
  [@@deriving validator, show, eq]
end = struct
  type t = { min_module : string [@min_length 2] }
  [@@deriving validator, show, eq]
end

let _ = Test.show

type other_test_record = { other_min : string [@min_length 2] }
[@@deriving validator, show, eq]

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
[@@deriving validator, show, eq]

(* type aaa = (string list list[@min_length 2]) [@@deriving validator] *)

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
       (Validator.RecordError
          [
            ( "other_test_record_list",
              [
                Validator.IterableError
                  [
                    ( 0,
                      [
                        Validator.RecordError
                          [
                            ( "other_min",
                              [
                                Validator.BaseError
                                  {
                                    code = "min_length";
                                    params = [ ("threshold", "2") ];
                                  };
                              ] );
                          ];
                      ] );
                  ];
                Validator.BaseError
                  { code = "min_length"; params = [ ("threshold", "2") ] };
              ] );
            ( "module_test_record",
              [
                Validator.RecordError
                  [
                    ( "min_module",
                      [
                        Validator.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "other_test_record",
              [
                Validator.RecordError
                  [
                    ( "other_min",
                      [
                        Validator.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "list_max_length",
              [
                Validator.IterableError
                  [
                    ( 5,
                      [
                        Validator.BaseError
                          {
                            Validator.code = "max_length";
                            params = [ ("threshold", "3") ];
                          };
                      ] );
                  ];
                Validator.BaseError
                  { code = "max_length"; params = [ ("threshold", "5") ] };
              ] );
            ( "list_min_length",
              [
                Validator.IterableError
                  [
                    ( 0,
                      [
                        Validator.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "1") ];
                          };
                      ] );
                  ];
                Validator.BaseError
                  { code = "min_length"; params = [ ("threshold", "3") ] };
              ] );
            ( "test_list",
              [
                Validator.IterableError
                  [
                    ( 2,
                      [
                        Validator.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                    ( 0,
                      [
                        Validator.BaseError
                          {
                            code = "min_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "option_some",
              [
                Validator.BaseError
                  { code = "min_length"; params = [ ("threshold", "2") ] };
              ] );
            ( "not_equal_to",
              [
                Validator.BaseError
                  { code = "value_not_equal_to"; params = [ ("value", "10") ] };
              ] );
            ( "equal_to",
              [
                Validator.BaseError
                  { code = "value_equal_to"; params = [ ("value", "5") ] };
              ] );
            ( "greater_than_or_equal",
              [
                Validator.BaseError
                  {
                    code = "value_greater_than_or_equal";
                    params = [ ("threshold", "5") ];
                  };
              ] );
            ( "greater_than",
              [
                Validator.BaseError
                  {
                    code = "value_greater_than";
                    params = [ ("threshold", "5") ];
                  };
              ] );
            ( "less_than_or_equal",
              [
                Validator.BaseError
                  {
                    code = "value_less_than_or_equal";
                    params = [ ("threshold", "10") ];
                  };
              ] );
            ( "less_than",
              [
                Validator.BaseError
                  { code = "value_less_than"; params = [ ("threshold", "10") ] };
              ] );
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
  run "Validator"
    [
      ( "deriving",
        [ test_case "Error" `Quick test_err; test_case "Ok" `Quick test_ok ] );
    ]
