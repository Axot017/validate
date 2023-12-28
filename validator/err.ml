let min_length_error_code = "min_length"
let max_length_error_code = "max_length"

type base_validation_error = { code : string; params : (string * string) list }
and field_validation_error = { field : string; field_error : validation_error }
and list_validation_error = { index : int; list_error : validation_error }

and validation_error =
  | Base of base_validation_error
  | Field of field_validation_error list
  | List of list_validation_error list
