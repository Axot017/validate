(lang dune 3.12)

(name validate)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name validate)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (= "5.1.0")) dune (alcotest :with-test) ppx_deriving re uri)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
