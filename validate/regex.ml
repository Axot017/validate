open Err

let uuid_regex =
  Re.Perl.re
    "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$"
  |> Re.compile

let ulid_regex = Re.Perl.re "^[0-9A-HJKMNP-TV-Z]{26}$" |> Re.compile

let ipv4_regex =
  Re.Perl.re
    {|^(\b25[0-5]|\b2[0-4][0-9]|\b[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$|}
  |> Re.compile

let ipv6_regex =
  Re.Perl.re
    {|^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$|}
  |> Re.compile

let mac_address_regex =
  Re.Perl.re {|^[a-fA-F0-9]{2}(:[a-fA-F0-9]{2}){5}$|} |> Re.compile

let phone_number_regex = Re.Perl.re {|^\+[1-9]\d{1,14}$|} |> Re.compile
let numeric_regex = Re.Perl.re "^[0-9]*$" |> Re.compile
let alpha_regex = Re.Perl.re "^[a-zA-Z]*$" |> Re.compile
let alphanumeric_regex = Re.Perl.re "^[a-zA-Z0-9]*$" |> Re.compile
let lowercase_regex = Re.Perl.re "^[a-z]*$" |> Re.compile
let uppercase_regex = Re.Perl.re "^[A-Z]*$" |> Re.compile
let lowercase_alphanumeric_regex = Re.Perl.re "^[a-z0-9]*$" |> Re.compile
let uppercase_alphanumeric_regex = Re.Perl.re "^[A-Z0-9]*$" |> Re.compile

let email_regex =
  Re.Perl.re
    "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$"
  |> Re.compile

let _validate_regex code regex str =
  let result = Re.execp regex str in
  if result then Ok () else Error (BaseError { code; params = [] })

let validate_regex regex str =
  _validate_regex invalid_pattern_error_code regex str

let validate_str_regex regex =
  let compiled_regex = Re.Perl.re regex |> Re.compile in
  fun str -> validate_regex compiled_regex str

let validate_uuid uuid = _validate_regex invalid_uuid_error_code uuid_regex uuid
let validate_ulid uuid = _validate_regex invalid_ulid_error_code ulid_regex uuid
let validate_ipv4 str = _validate_regex invalid_ipv4_error_code ipv4_regex str
let validate_ipv6 str = _validate_regex invalid_ipv6_error_code ipv6_regex str

let validate_mac_address str =
  _validate_regex invalid_mac_address_error_code mac_address_regex str

let validate_phone_number str =
  _validate_regex invalid_phone_number_error_code phone_number_regex str

let validate_numeric str =
  _validate_regex invalid_numeric_error_code numeric_regex str

let validate_alpha str =
  _validate_regex invalid_alpha_error_code alpha_regex str

let validate_alphanumeric str =
  _validate_regex invalid_alphanumeric_error_code alphanumeric_regex str

let validate_lowercase str =
  _validate_regex invalid_lowercase_error_code lowercase_regex str

let validate_uppercase str =
  _validate_regex invalid_uppercase_error_code uppercase_regex str

let validate_lowercase_alphanumeric str =
  _validate_regex invalid_lowercase_alphanumeric_error_code
    lowercase_alphanumeric_regex str

let validate_uppercase_alphanumeric str =
  _validate_regex invalid_uppercase_alphanumeric_error_code
    uppercase_alphanumeric_regex str

let validate_email str =
  _validate_regex invalid_email_error_code email_regex str
