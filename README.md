# Validate

**Note**: This is a preview version of the `validate` library. 
It is still under development, and users may encounter errors. 
Feedback and contributions are highly appreciated during this stage.

## Overview
`validate` is an OCaml library designed to streamline the process of validating records, lists, 
or values. It primarily operates through a PPX deriver that automatically generates 
validators using annotations, utilizing an underlying library of helper validation functions.

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

```ocaml
type test_record = {
  min : string; [@min_length 2]
  max : string; [@max_length 5]
  numeric_list : int list; [@list_min_length 2] [@less_than 10]
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

### Categorized Annotations

String Annotations

- `@min_length`: Validates minimum length of a string.
- `@max_length`: Validates maximum length of a string.
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
- `@regex`: Checks if string matches given regex

Integer/Float Annotations

- `@less_than`: Validates an integer/float to be less than a specified value.
- `@less_than_or_equal`: Validates an integer/float to be less than or equal to a specified value.
- `@greater_than`: Validates an integer/float to be greater than a specified value.
- `@greater_than_or_equal`: Validates an integer/float to be greater than or equal to a specified value.
- `@equal_to`: Validates an integer/float to be equal to a specified value.
- `@not_equal_to`: Validates an integer/float to not be equal to a specified value.

List Annotations

- `@list_min_length`: Validates the minimum length of a list.
- `@list_max_length`: Validates the maximum length of a list.

Annotations for Other Types

- `@dive`: Used for nested record validation, allowing validation of each element within a composite structure like records or lists.

## Contributing

Contributions to validate are warmly welcomed and appreciated. 
Whether it's opening issues for bug reports, suggesting new features, or submitting pull requests, 
all forms of contribution help in making validate better.
