# Validate

[![codecov](https://codecov.io/gh/Axot017/validate/graph/badge.svg?token=6RF8IIBDNN)](https://codecov.io/gh/Axot017/validate)
[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/Axot017/validate/test.yml)](https://github.com/Axot017/validate/actions/workflows/test.yml)
[![GitHub Release](https://img.shields.io/github/v/release/Axot017/validate)](https://github.com/Axot017/validate/releases)
[![GitHub License](https://img.shields.io/github/license/Axot017/validate)](https://github.com/Axot017/validate/blob/master/LICENSE)

## Overview
`validate` is an OCaml library designed to streamline the process of validating records, lists, 
or values. It primarily operates through a PPX deriver that automatically generates 
validators using annotations, utilizing an underlying library of helper validation functions.

## Prerequisites

- OCaml version 5.0 or higher.

## Installation

### Installing `validate`
To install the `validate` library, use OPAM with the following command:
```
opam install validate
```

After installation, you need to add `validate` as a library in your project's dune file and specify `validate.ppx_derive_validate` as a preprocessor. 
Here is an example of how to set up the dune file:

```lisp
(library
  (name something)
  (preprocess (pps validate.ppx_derive_validate))
  (libraries validate))
```

## Annotations and Usage

The `validate` library in OCaml allows for precise validation of data through a variety of annotations. 
For each type, the library generates a function named `validate_[type_name]` which can be used to perform the validation.

### Example Usage

#### Validating record
```ocaml
type test_record = {
  min : string; [@min_length 2]
  max : string; [@max_length 5]
  numeric_list : (int [@less_than 10]) list; [@min_length 2] 
  ...
}
[@@deriving validate, show, eq]

let example_record = { min = "ab"; max = "hello"; numeric_list = [1, 2, 3] }
let validation_result = validate_test_record example_record
```

In this example:

- `min` is a string field validated for a minimum length of 2.
- `max` is validated for a maximum length of 5.
- `numeric_list` is an integer list, validated for a minimum length of 2, and each element is validated to be less than 10.

#### Validating simple types
```ocaml
type list_type = ((string[@min_length 1]) list[@min_length 2]) [@@deriving validate]

let example_list = ["a"; "bc"]
let validation_result = validate_list_type example_list
```
In this example, validate_list_type function will validate that each string in the list has a minimum length of 1 and that the list itself has a minimum length of 2.

#### Validating tuples
```ocaml
type tuple_type = (string[@email]) * (int[@greater_than 1]) [@@deriving validate]

let example_tuple = ("example@email.com", 2)
let validation_result = validate_tuple_type example_tuple
```
Here, the validate_tuple_type function ensures the first element of the tuple is a valid email address and the second element is an integer greater than 1.

#### Validating tuples
Variants in OCaml can also be validated using `validate`. Here's an example of how to use annotations with variants:
```ocaml
type tuple_variant =
  | EmailToId of (string[@email]) * (int[@greater_than_or_equal 0])
  | Email of (string[@email])
  | Profile of {
    username : string [@min_length 3];
    email : string [@email];
  }
[@@deriving validate]

(* Example usage *)
let email_to_id_variant = EmailToId ("example@email.com", 0)
let validation_result = validate_tuple_variant email_to_id_variant
```
In this example:

- `EmailToId` is a variant that takes a tuple. The first element is validated as an email, and the second as an integer greater than or equal to 0.
- `Email` is a single-element variant validated as an email.
- `Profile` is a record variant with `username` validated for a minimum length of 3, and `email` validated as a valid email address.

#### Validating Recursive Types
`validate` also supports recursive types, allowing for the validation of nested, self-referential data structures. Here is an example demonstrating the validation of a recursive type representing a binary tree:

```ocaml
type tree =
  | Leaf of (int[@greater_than 0])
  | Node of { left : tree; [@dive] right : (tree[@dive]) }
[@@deriving validate, show, eq]

(* Example usage *)
let my_tree = Node { left = Leaf 1; right = Leaf 2 }
let validation_result = validate_tree my_tree
```
In this example:

- `Leaf` is a variant that takes an integer, validated to be greater than 0.
- `Node` is a variant representing a binary tree node with `left` and `right` branches, both of which are recursively validated as `tree` instances.

#### Validating Circular Recursive Types
`validate` also handles circular recursive types, which are useful for defining structures where two types refer to each other recursively. This feature is particularly useful for complex data models. Here's an example:

```ocaml
type a = { 
  a_id : int; [@greater_than 0] 
  b : (b[@dive]) option 
}
[@@deriving validate, show, eq]

and b = { 
  b_id : int; [@greater_than 0] 
  a : (a[@dive]) option 
}
[@@deriving validate, show, eq]

(* Example usage *)
let rec a_instance = { a_id = 1; b = Some { b_id = 2; a = Some a_instance } }
let validation_result_a = validate_a a_instance

let rec b_instance = { b_id = 1; a = Some { a_id = 2; b = Some b_instance } }
let validation_result_b = validate_b b_instance
```

In this example:

- Type `a` has an integer field `a_id` validated to be greater than 0, and an optional field `b` of type `b`.
- Type `b` similarly has a `b_id` field and an optional field `a` of type `a`.
- Both types use the `[@dive]` annotation to indicate recursive validation within their optional fields.

### Categorized Annotations

String/List Annotations

- `@min_length`: Validates minimum length of a string/list.
- `@max_length`: Validates maximum length of a string/list.
- `@length_equals`: Validates length of a string/list.

String Annotations

- `@url`: Checks if a string is a valid URL.
- `@uuid`: Validates a string as a UUID.
- `@numeric`: Ensures a string contains only numeric characters.
- `@alpha`: Checks for alphabetic characters in a string.
- `@alphanumeric`: Requires a string to be alphanumeric.
- `@lowercase`: Validates a string to be in lowercase.
- `@uppercase`: Ensures a string is in uppercase.
- `@lowercase_alphanumeric`: Validates a lowercase alphanumeric string.
- `@uppercase_alphanumeric`: Validates an uppercase alphanumeric string.
- `@email`: Checks if a string is a valid email.
- `@regex`: Checks if string matches given regex. Note: Under the hood it uses [ocaml-re](https://github.com/ocaml/ocaml-re) which does not support back-references and look-ahead/look-behind assertions. 
- `@ulid`: Ensures a string is a valid Universally Unique Lexicographically Sortable Identifier (ULID).
- `@ipv4`: Validates that a string is a valid IPv4 address.
- `@ipv6`: Ensures a string is a valid IPv6 address.
- `@phone`: Validates that a string conforms to the E.164 international phone number format.
- `@mac_address`: Ensures a string is a valid MAC address.


Integer/Float Annotations

- `@less_than`: Validates an integer/float to be less than a specified value.
- `@less_than_or_equal`: Validates an integer/float to be less than or equal to a specified value.
- `@greater_than`: Validates an integer/float to be greater than a specified value.
- `@greater_than_or_equal`: Validates an integer/float to be greater than or equal to a specified value.
- `@equal_to`: Validates an integer/float to be equal to a specified value.
- `@not_equal_to`: Validates an integer/float to not be equal to a specified value.


Annotations for Other Types

- `@dive`: Used for nested record validation, allowing validation of each element within a composite structure like records or lists.

Option Type Annotations

- `@some`: Ensures that an option type is Some, indicating a value is present.
- `@none`: Ensures that an option type is None, indicating no value is present.

Advanced Annotations

- `@custom`: This annotation allows you to define custom validation logic. You provide a function that takes one argument and returns a result, which is either `Ok ()` for successful validation or `Error validation_error` for a validation failure.

```ocaml
let custom_validator str =
  if String.length str > 1 then Ok ()
  else Error (Validate.BaseError { code = "custom_validator"; params = [] })

type custom_validator_record = {
  custom_validator : string; [@custom custom_validator]
  custom_inline_validator : int;
      [@custom
        fun i ->
          if i > 1 then Ok ()
          else
            Error
              (Validate.BaseError { code = "custom_validator"; params = [] })]
}
[@@deriving validate]
```

- `@ignore_if`: This annotation accepts a function that takes the type itself as an argument and returns a boolean. If the function returns true, other validators for the field are ignored.

```ocaml
type cond_record = {
  unit : string;
  temperature : int; [@greater_than_or_equal 0] [@ignore_if fun r -> r.unit <> "K"]
}
[@@deriving validate]
```

- `@some_if` and `@none_if`: These annotations are applicable only to option types. `@some_if` requires the option to be `Some` if the provided function returns true, and `@none_if` requires it to be `None` under the specified condition.

```ocaml
type username_or_email = {
  username : string option; [@some_if fun r -> r.email = None]
  email : string option; [@none_if fun r -> Option.is_some r.username]
}
[@@deriving validate]
```

## Error Handling

In validate, the validation function returns a result type, which includes an Ok value equal to the input parameter, or an error parameter. 
The error types are defined as follows:

- `base_validation_error`: Represents a basic validation error with a code and a list of parameters.

```ocaml
type base_validation_error = { 
  code : string; 
  params : (string * string) list 
}
```

- `keyed_validation_errors`: Represents errors associated with specific keys e.g. for records key equal to field name and for tuples key is position in tuple.
```ocaml
type keyed_validation_errors = string * validation_error list
```

- `index_validation_error`:  Represents errors indexed by an integer, typically for list validations.
```ocaml
type index_validation_error = int * validation_error list
```
- `validation_error`: The main error type which can be one of the following:
    - `BaseError`: Basic validation error.
    - `KeyedError`: Errors associated with specific keys.
    - `IterableError`: Errors in iterable structures like lists.
    - `GroupError`: A group of nested validation errors.
```ocaml
type validation_error =
  | BaseError of base_validation_error
  | KeyedError of keyed_validation_errors list
  | IterableError of index_validation_error list
  | GroupError of validation_error list
```

## Contributing

Contributions to validate are warmly welcomed and appreciated. 
Whether it's opening issues for bug reports, suggesting new features, or submitting pull requests, 
all forms of contribution help in making validate better.
