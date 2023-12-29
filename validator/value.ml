open Err

let validate_equal other value =
  if other = value then Ok value
  else
    Error
      (BaseError
         { code = value_equals_error_code; params = [ ("value", other) ] })

let validate_not_equal other value =
  if other <> value then Ok value
  else
    Error
      (BaseError
         { code = value_not_equals_error_code; params = [ ("value", other) ] })

let validate_greater_than other value =
  if value > other then Ok value
  else
    Error
      (BaseError
         {
           code = value_greater_than_error_code;
           params = [ ("threshold", other) ];
         })

let validate_greater_than_or_equal other value =
  if value >= other then Ok value
  else
    Error
      (BaseError
         {
           code = value_greater_than_or_equal_error_code;
           params = [ ("threshold", other) ];
         })

let validate_less_than other value =
  if value < other then Ok value
  else
    Error
      (BaseError
         {
           code = value_less_than_error_code;
           params = [ ("threshold", other) ];
         })

let validate_less_than_or_equal other value =
  if value <= other then Ok value
  else
    Error
      (BaseError
         {
           code = value_less_than_or_equal_error_code;
           params = [ ("threshold", other) ];
         })
