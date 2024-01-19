let () =
  let open Alcotest in
  run "Validate" [ Record.t; Abstract.t; Tuple.t (* Variant.t *) ]
