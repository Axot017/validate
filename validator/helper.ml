open Err

type 'a validator = 'a -> (unit, validation_error) result
and ('a, 'b) field_extractor = 'a -> 'b
and 'a field_validator = 'a -> (unit, field_validation_error) result

let field field_name (field_extractor : ('a, 'b) field_extractor)
    (validators : 'b validator list) record :
    (unit, field_validation_error) result =
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

let record (validators : 'a field_validator list) record :
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
  match errors with [] -> Ok () | errors -> Error (RecordError errors)

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
