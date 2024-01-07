open Err

let validate_equal_to to_str other value =
  if other = value then Ok ()
  else
    Error
      (BaseError
         {
           code = value_equal_to_error_code;
           params = [ ("value", to_str other) ];
         })

let validate_not_equal_to to_str other value =
  if other <> value then Ok ()
  else
    Error
      (BaseError
         {
           code = value_not_equal_to_error_code;
           params = [ ("value", to_str other) ];
         })

let validate_greater_than to_str other value =
  if value > other then Ok ()
  else
    Error
      (BaseError
         {
           code = value_greater_than_error_code;
           params = [ ("threshold", to_str other) ];
         })

let validate_greater_than_or_equal to_str other value =
  if value >= other then Ok ()
  else
    Error
      (BaseError
         {
           code = value_greater_than_or_equal_error_code;
           params = [ ("threshold", to_str other) ];
         })

let validate_less_than to_str other value =
  if value < other then Ok ()
  else
    Error
      (BaseError
         {
           code = value_less_than_error_code;
           params = [ ("threshold", to_str other) ];
         })

let validate_less_than_or_equal to_str other value =
  if value <= other then Ok ()
  else
    Error
      (BaseError
         {
           code = value_less_than_or_equal_error_code;
           params = [ ("threshold", to_str other) ];
         })
