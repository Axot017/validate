open Err

let validate_min_length get_len min_length value =
  let len = get_len value in
  if len < min_length then
    Error
      {
        code = min_length_error_code;
        params = [ ("threshold", string_of_int min_length) ];
      }
  else Ok value

let validate_max_length get_len max_length value =
  let len = get_len value in
  if len > max_length then
    Error
      {
        code = max_length_error_code;
        params = [ ("threshold", string_of_int max_length) ];
      }
  else Ok value
