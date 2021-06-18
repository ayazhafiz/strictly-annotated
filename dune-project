(lang dune 2.8)
(name strictly-annotated)
(version 0.1.0)

(generate_opam_files true)

(source (github ayazhafiz/strictly-annotated))
(license MIT)
(authors "Ayaz Hafiz")
(maintainers "ayaz.hafiz.1@gmail.com")

(package
 (name strictly-annotated)
 (synopsis "A pretty-printer for documents and annotations")
 (description "\
Strictly Annotated provides a standard pretty-printer interface for documents,
with the additional capacity to cohesively render annotations (like comments)
attached to arbitrary nodes in a document.
")
 (depends
  (alcotest :with-test)))
