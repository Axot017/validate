let () =
  let open Alcotest in
  run "Validate" (Length.t @ Helper.t @ Custom.t @ Regex.t)
