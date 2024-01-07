open Err

let validate_min_length get_len min_length value =
  let len = get_len value in
  if len < min_length then
    Error
      (BaseError
         {
           code = min_length_error_code;
           params = [ ("threshold", string_of_int min_length) ];
         })
  else Ok ()

let validate_max_length get_len max_length value =
  let len = get_len value in
  if len > max_length then
    Error
      (BaseError
         {
           code = max_length_error_code;
           params = [ ("threshold", string_of_int max_length) ];
         })
  else Ok ()

let validate_length_between get_len ~min ~max value =
  let res = validate_min_length get_len min value in
  match res with
  | Ok () -> validate_max_length get_len max value
  | Error _ -> res

let validate_length_equals get_len length value =
  let len = get_len value in
  if len <> length then
    Error
      (BaseError
         {
           code = length_equals_error_code;
           params = [ ("value", string_of_int length) ];
         })
  else Ok ()
