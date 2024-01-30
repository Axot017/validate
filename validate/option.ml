let validate_some (value : 'a option) =
  match value with
  | None ->
      Error (Err.BaseError { code = Err.expect_some_error_code; params = [] })
  | Some _ -> Ok ()

let validate_some_if (cond : bool) (value : 'a option) =
  match (cond, value) with
  | true, None ->
      Error (Err.BaseError { code = Err.expect_some_error_code; params = [] })
  | false, Some _ ->
      Error (Err.BaseError { code = Err.expect_none_error_code; params = [] })
  | _ -> Ok ()

let validate_none (value : 'a option) =
  match value with
  | None -> Ok ()
  | Some _ ->
      Error (Err.BaseError { code = Err.expect_none_error_code; params = [] })

let validate_none_if (cond : bool) (value : 'a option) =
  match (cond, value) with
  | true, Some _ ->
      Error (Err.BaseError { code = Err.expect_none_error_code; params = [] })
  | false, None ->
      Error (Err.BaseError { code = Err.expect_some_error_code; params = [] })
  | _ -> Ok ()
