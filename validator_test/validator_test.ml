let () =
  let open Alcotest in
  run "Validator" (Length.t @ Helper.t @ Custom.t)
