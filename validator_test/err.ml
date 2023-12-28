let pp_base_validation_error ppf (e : Validator.base_validation_error) =
  let fmt_pair =
    Fmt.pair ~sep:(Fmt.const Fmt.string "=") Fmt.string Fmt.string
  in
  let fmt_list_of_pairs = Fmt.list ~sep:(Fmt.const Fmt.string ", ") fmt_pair in
  Fmt.pf ppf "{ code = %s; params = [%a] }" e.code fmt_list_of_pairs e.params

let equal_base_validation_error (e1 : Validator.base_validation_error)
    (e2 : Validator.base_validation_error) =
  e1.code = e2.code
  && List.for_all2
       (fun (k1, v1) (k2, v2) -> k1 = k2 && v1 = v2)
       e1.params e2.params

let base_validation_error_testable =
  Alcotest.testable pp_base_validation_error equal_base_validation_error
