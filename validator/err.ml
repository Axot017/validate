(* Lengt *)
let min_length_error_code = "min_length"
let max_length_error_code = "max_length"
let length_equals_error_code = "length_equals"

(* Value *)
let value_equals_error_code = "value_equals"
let value_not_equals_error_code = "value_not_equals"
let value_greater_than_error_code = "value_greater_than"
let value_greater_than_or_equal_error_code = "value_greater_than_or_equal"
let value_less_than_error_code = "value_less_than"
let value_less_than_or_equal_error_code = "value_less_than_or_equal"
let value_between_error_code = "value_between"

(* Regex *)
let invalid_pattern_error_code = "invalid_pattern"
let invalid_uuid_error_code = "invalid_uuid"
let invalid_numeric_error_code = "invalid_numeric"
let invalid_alpha_error_code = "invalid_alpha"
let invalid_alphanumeric_error_code = "invalid_alphanumeric"
let invalid_lowercase_error_code = "invalid_lowercase"
let invalid_uppercase_error_code = "invalid_uppercase"
let invalid_lowercase_alphanumeric_error_code = "invalid_lowercase_alphanumeric"
let invalid_uppercase_alphanumeric_error_code = "invalid_uppercase_alphanumeric"

(* Custom *)
let invalid_url_error_code = "invalid_url"

type base_validation_error = { code : string; params : (string * string) list }
[@@deriving show, eq]

and field_validation_error = string * validation_error list
[@@deriving show, eq]

and index_validation_error = int * validation_error list [@@deriving show, eq]

and validation_error =
  | BaseError of base_validation_error
  | RecordError of field_validation_error list
  | IterableError of index_validation_error list
[@@deriving show, eq]
