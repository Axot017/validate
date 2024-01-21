open Err

type 'a validator = 'a -> (unit, validation_error) result
and ('a, 'b) field_extractor = 'a -> 'b
and ('a, 'b) named_value_extractor = 'a -> 'b option
and 'a keyed_validator = 'a -> (unit, keyed_validation_errors) result

let validate (validator : 'a validator) (value : 'a) :
    ('a, validation_error) result =
  match validator value with Ok _ -> Ok value | Error error -> Error error

let field field_name (field_extractor : ('a, 'b) field_extractor)
    (validators : 'b validator list) record :
    (unit, keyed_validation_errors) result =
  let value = field_extractor record in
  let rec validate validators errors =
    match validators with
    | [] -> errors
    | validator :: rest -> (
        match validator value with
        | Ok _ -> validate rest errors
        | Error error -> validate rest (error :: errors))
  in
  let errors = validate validators [] in
  match errors with [] -> Ok () | errors -> Error (field_name, errors)

let keyed (validators : 'a keyed_validator list) record :
    (unit, validation_error) result =
  let rec validate validators errors =
    match validators with
    | [] -> errors
    | validator :: rest -> (
        match validator record with
        | Ok _ -> validate rest errors
        | Error error -> validate rest (error :: errors))
  in
  let errors = validate validators [] in
  match errors with [] -> Ok () | errors -> Error (KeyedError errors)

let named_value name (extractor : ('a, 'b) named_value_extractor)
    (validators : 'b validator list) variant :
    (unit, keyed_validation_errors) result =
  let value = extractor variant in
  match value with
  | Some value -> (
      let rec validate validators errors =
        match validators with
        | [] -> errors
        | validator :: rest -> (
            match validator value with
            | Ok _ -> validate rest errors
            | Error error -> validate rest (error :: errors))
      in
      let errors = validate validators [] in
      match errors with [] -> Ok () | errors -> Error (name, errors))
  | None -> Ok ()

let iterable_item index (validators : 'a validator list) item :
    (unit, index_validation_error) result =
  let rec validate validators errors =
    match validators with
    | [] -> errors
    | validator :: rest -> (
        match validator item with
        | Ok _ -> validate rest errors
        | Error error -> validate rest (error :: errors))
  in
  match validate validators [] with
  | [] -> Ok ()
  | errors -> Error (index, errors)

let list (validators : 'a validator list) iterable :
    (unit, validation_error) result =
  let rec validate iterable errors index =
    match iterable with
    | [] -> errors
    | item :: rest -> (
        match iterable_item index validators item with
        | Ok _ -> validate rest errors (index + 1)
        | Error error -> validate rest (error :: errors) (index + 1))
  in
  match validate iterable [] 0 with
  | [] -> Ok ()
  | errors -> Error (IterableError errors)

let ignore_ok f v =
  let result = f v in
  match result with Ok _ -> Ok () | Error _ as error -> error

let group (validators : 'a validator list) value =
  let rec validate validators errors =
    match validators with
    | [] -> errors
    | validator :: rest -> (
        match validator value with
        | Ok _ -> validate rest errors
        | Error error -> validate rest (error :: errors))
  in
  match validate validators [] with
  | [] -> Ok ()
  | errors -> Error (GroupError errors)

let option (validators : 'a validator list) : 'a option validator = function
  | Some value -> group validators value
  | None -> Ok ()
