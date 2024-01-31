## v1.1.0 (2024-02-01)

- Added new string annotations: 
  - `@ulid`: Ensures a string is a valid ULID.
  - `@ipv4`: Validates that a string is a valid IPv4 address.
  - `@ipv6`: Ensures a string is a valid IPv6 address.
  - `@phone`: Validates that a string conforms to the E.164 phone number format.
  - `@mac_address`: Ensures a string is a valid MAC address.
- Introduced new option type annotations: 
  - `@some`: Ensures an option type is `Some`.
  - `@none`: Ensures an option type is `None`.
- Implemented `@custom` annotation for custom validation logic.
- Added `@ignore_if` annotation for conditional validation.
- Introduced `@some_if` and `@none_if` annotations for conditional requirements on option types.

## v1.0.0 (2024-01-26)

- Added support for validating variants.
- Introduced support for recursive types, enabling the validation of nested self-referential data structures.
- Implemented support for circular recursive types, allowing for validation in complex interconnected data structures.
- API stabilization: The API can now be considered stable. Future changes will primarily focus on adding new validators without altering existing functionality.

## v0.2.0 (2024-01-17)

- Support for simple types
- Support for tuples
- Group error type added

## v0.1.0 (2024-01-09)

- Initial release
