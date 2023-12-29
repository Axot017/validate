let pp_validation_error ppf (err : Validator.validation_error) =
  Format.fprintf ppf "%s" (Validator.show_validation_error err)

let validation_error_testable =
  Alcotest.testable pp_validation_error Validator.equal_validation_error
