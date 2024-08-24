(lang dune 3.16)

(name typed_latex)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name typed_latex)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core stdio menhir ppx_deriving re)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
