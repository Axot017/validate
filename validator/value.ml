open Err

let validate_equal f expected actual =
  if f expected actual then Ok actual
  else
    Error { code = value_equals_error_code; params = [ ("value", expected) ] }
