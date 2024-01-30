let () =
  let open Alcotest in
  run "Validate"
    [ Abstract.t; Record.t; Tuple.t; Variant.t; Recursive.t; Conditional.t ]
