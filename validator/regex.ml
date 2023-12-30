open Err

let uuid_regex =
  Re.Perl.re
    "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"
  |> Re.compile

let numeric_regex = Re.Perl.re "[0-9]+" |> Re.compile
let alpha_regex = Re.Perl.re "[a-zA-Z]+" |> Re.compile
let alpha_numeric_regex = Re.Perl.re "[a-zA-Z0-9]+" |> Re.compile
let lowercase_regex = Re.Perl.re "[a-z]+" |> Re.compile
let uppercase_regex = Re.Perl.re "[A-Z]+" |> Re.compile
let lowercase_alpha_numeric_regex = Re.Perl.re "[a-z0-9]+" |> Re.compile
let uppercase_alpha_numeric_regex = Re.Perl.re "[A-Z0-9]+" |> Re.compile

let _validate_regex code regex str =
  let result = Re.execp regex str in
  if result then Ok () else Error (BaseError { code; params = [] })

let validate_regex regex str =
  _validate_regex invalid_pattern_error_code regex str

let validate_uuid uuid = _validate_regex invalid_uuid_error_code uuid_regex uuid

let validate_numeric str =
  _validate_regex invalid_numeric_error_code numeric_regex str

let validate_alpha str =
  _validate_regex invalid_alpha_error_code alpha_regex str

let validate_alpha_numeric str =
  _validate_regex invalid_alpha_numeric_error_code alpha_numeric_regex str

let validate_lowercase str =
  _validate_regex invalid_lowercase_error_code lowercase_regex str

let validate_uppercase str =
  _validate_regex invalid_uppercase_error_code uppercase_regex str

let validate_lowercase_alpha_numeric str =
  _validate_regex invalid_lowercase_alpha_numeric_error_code
    lowercase_alpha_numeric_regex str

let validate_uppercase_alpha_numeric str =
  _validate_regex invalid_uppercase_alpha_numeric_error_code
    uppercase_alpha_numeric_regex str
