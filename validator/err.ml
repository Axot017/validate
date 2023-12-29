let min_length_error_code = "min_length"
let max_length_error_code = "max_length"
let length_equals_error_code = "length_equals"
let value_equals_error_code = "value_equals"

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
