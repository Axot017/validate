open Err

let validate_url url_string =
  let uri = Uri.of_string url_string in
  match (Uri.scheme uri, Uri.host uri) with
  | Some _, Some _ -> Ok ()
  | _ -> Error (BaseError { code = invalid_url_error_code; params = [] })
