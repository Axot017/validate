(lang dune 3.12)

(name validator)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name validator)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (= "5.1.0")) dune (alcotest :with-test) ppx_deriving)
 (tags
  (topics "to describe" your project)))
(package
 (name ppx_derive_validator)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (= "5.1.0")) dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
