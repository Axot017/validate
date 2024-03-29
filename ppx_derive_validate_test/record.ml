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

let custom_validator str =
  if String.length str > 1 then Ok ()
  else Error (Validate.BaseError { code = "custom_validator"; params = [] })

type test_record = {
  min : (string[@min_length 2]);
  max : string; [@max_length 5]
  equals : string; [@length_equals 5]
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
  option_some : (string[@min_length 2]) option;
  option_none : (string[@min_length 2]) option;
  test_list : (string[@min_length 2]) list;
  list_min_length : (string[@min_length 1]) list; [@min_length 3]
  list_max_length : (string[@max_length 3]) list; [@max_length 5]
  other_test_record : other_test_record; [@dive]
  module_test_record : Test.t; [@dive]
  other_test_record_list : (other_test_record[@dive]) list; [@min_length 2]
  email : string; [@email]
  regex : string; [@regex "^test[a-z]+$"]
  nested_list : ((string[@max_length 1]) list[@max_length 1]) list;
      [@max_length 1]
  ulid : string; [@ulid]
  ipv4 : string; [@ipv4]
  ipv6 : string; [@ipv6]
  phone : string; [@phone]
  mac_address : string; [@mac_address]
  custom_validator : string; [@custom custom_validator]
  custom_inline_validator : int;
      [@custom
        fun i ->
          if i > 1 then Ok ()
          else
            Error
              (Validate.BaseError { code = "custom_validator"; params = [] })]
  some : string option; [@some]
  none : string option; [@none]
}
[@@deriving validate, show, eq]

let test_record_testable = Alcotest.testable pp_test_record equal_test_record

let test_err () =
  let result =
    validate_test_record
      {
        min = "1";
        max = "123456";
        equals = "123456";
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
        email = "invalid email";
        regex = "invalid regex";
        nested_list =
          [ [ "111"; "2" ]; [ "12" ]; [ "1" ]; [ "111"; "fffff"; "123" ] ];
        ulid = "invalid ulid";
        ipv4 = "invalid ipv4";
        ipv6 = "invalid ipv6";
        phone = "invalid phone";
        mac_address = "invalid mac address";
        custom_validator = "1";
        custom_inline_validator = 1;
        some = None;
        none = Some "1";
      }
  in
  Alcotest.(check (result test_record_testable Error.validation_error_testable))
    "returns Error"
    (Error
       (Validate.KeyedError
          [
            ( "none",
              [ Validate.BaseError { code = "expect_none"; params = [] } ] );
            ( "some",
              [ Validate.BaseError { code = "expect_some"; params = [] } ] );
            ( "custom_inline_validator",
              [ Validate.BaseError { code = "custom_validator"; params = [] } ]
            );
            ( "custom_validator",
              [ Validate.BaseError { code = "custom_validator"; params = [] } ]
            );
            ( "mac_address",
              [
                Validate.BaseError { code = "invalid_mac_address"; params = [] };
              ] );
            ( "phone",
              [
                Validate.BaseError
                  { code = "invalid_phone_number"; params = [] };
              ] );
            ( "ipv6",
              [ Validate.BaseError { code = "invalid_ipv6"; params = [] } ] );
            ( "ipv4",
              [ Validate.BaseError { code = "invalid_ipv4"; params = [] } ] );
            ( "ulid",
              [ Validate.BaseError { code = "invalid_ulid"; params = [] } ] );
            ( "nested_list",
              [
                Validate.BaseError
                  { code = "max_length"; params = [ ("threshold", "1") ] };
                Validate.IterableError
                  [
                    ( 3,
                      [
                        Validate.BaseError
                          {
                            code = "max_length";
                            params = [ ("threshold", "1") ];
                          };
                        Validate.IterableError
                          [
                            ( 2,
                              [
                                Validate.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "1") ];
                                  };
                              ] );
                            ( 1,
                              [
                                Validate.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "1") ];
                                  };
                              ] );
                            ( 0,
                              [
                                Validate.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "1") ];
                                  };
                              ] );
                          ];
                      ] );
                    ( 1,
                      [
                        Validate.IterableError
                          [
                            ( 0,
                              [
                                Validate.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "1") ];
                                  };
                              ] );
                          ];
                      ] );
                    ( 0,
                      [
                        Validate.BaseError
                          {
                            code = "max_length";
                            params = [ ("threshold", "1") ];
                          };
                        Validate.IterableError
                          [
                            ( 0,
                              [
                                Validate.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "1") ];
                                  };
                              ] );
                          ];
                      ] );
                  ];
              ] );
            ( "regex",
              [ Validate.BaseError { code = "invalid_pattern"; params = [] } ]
            );
            ( "email",
              [ Validate.BaseError { code = "invalid_email"; params = [] } ] );
            ( "other_test_record_list",
              [
                Validate.BaseError
                  { code = "min_length"; params = [ ("threshold", "2") ] };
                Validate.IterableError
                  [
                    ( 0,
                      [
                        Validate.KeyedError
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
              ] );
            ( "module_test_record",
              [
                Validate.KeyedError
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
                Validate.KeyedError
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
                Validate.BaseError
                  { code = "max_length"; params = [ ("threshold", "5") ] };
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
              ] );
            ( "list_min_length",
              [
                Validate.BaseError
                  { code = "min_length"; params = [ ("threshold", "3") ] };
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
                Validate.GroupError
                  [
                    Validate.BaseError
                      { code = "min_length"; params = [ ("threshold", "2") ] };
                  ];
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
            ( "equals",
              [
                Validate.BaseError
                  { code = "length_equals"; params = [ ("value", "5") ] };
              ] );
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
      equals = "12345";
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
      email = "example@gmail.com";
      regex = "testa";
      nested_list = [ [ "1" ] ];
      ulid = "01E2WQZJXZJZJZJZJZJZJZJZJZ";
      ipv4 = "192.168.0.255";
      ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334";
      phone = "+12025550176";
      mac_address = "01:23:45:67:89:ab";
      custom_validator = "12";
      custom_inline_validator = 2;
      some = Some "12";
      none = None;
    }
  in
  let result = validate_test_record r in
  Alcotest.(check (result test_record_testable Error.validation_error_testable))
    "returns Ok" (Ok r) result

let t =
  let open Alcotest in
  ( "record",
    [ test_case "Error" `Quick test_err; test_case "Ok" `Quick test_ok ] )
