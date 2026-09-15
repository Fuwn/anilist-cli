(lang dune 3.22)
(name anilist)
(generate_opam_files true)

(package
 (name anilist)
 (synopsis "Command-line client for the AniList GraphQL API")
 (description "An OCaml command-line client for AniList's GraphQL API. It lowers concise CLI arguments into GraphQL operations, executes them against AniList, and prints JSON responses. It also supports schema introspection, variables, fragments, and custom HTTP headers.")
 (maintainers "Fuwn <contact@fuwn.net>")
 (authors "Fuwn <contact@fuwn.net>")
 (license AGPL-3.0-only)
 (homepage "https://github.com/Fuwn/anilist-cli")
 (bug_reports "https://github.com/Fuwn/anilist-cli/issues")
 (depends
  (ocaml (>= 5.0))
  cohttp-lwt-unix
  lwt_ssl
  (conf-python-3 :with-test)
  lwt
  uri
  yojson))
