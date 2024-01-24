(lang dune 3.12)

(name validate)

(generate_opam_files true)

(source
 (github Axot017/validate))

(authors "Mateusz Ledwoń <mateuszledwon@duck.com>")

(maintainers "Mateusz Ledwoń <mateuszledwon@duck.com>")

(license MIT)

(documentation https://axot017.github.io/validate/)

(package
 (name validate)
 (synopsis "OCaml library enabling efficient data validation through PPX derivers and a suite of annotation-based validators")
 (description "Validate is an OCaml library that focuses on data validation using PPX derivers and a range of annotations for different data types. It allows developers to apply annotations for various validation rules, such as string length, numeric values, and format constraints like URLs and UUIDs. This functionality makes it suitable for a wide array of applications in OCaml development where data integrity is crucial.")
 (depends (ocaml (>= "5.0.0")) dune (alcotest :with-test) ppx_deriving re uri (bisect_ppx :with-test))
 (tags
  (validation ppx)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
