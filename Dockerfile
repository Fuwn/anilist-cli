FROM ocaml/opam:debian-12-ocaml-5.2 AS build

WORKDIR /home/opam/anilist

RUN sudo apt-get update \
 && sudo apt-get install -y --no-install-recommends libgmp-dev libssl-dev m4 pkg-config python3 \
 && sudo rm -rf /var/lib/apt/lists/*

COPY --chown=opam:opam anilist.opam dune-project ./

RUN opam install . --deps-only --with-test -y

COPY --chown=opam:opam . .

RUN opam exec -- dune build @install \
 && opam exec -- dune runtest

FROM debian:12-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates libssl3 netbase \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /home/opam/anilist/_build/install/default/bin/anilist /usr/local/bin/anilist

ENTRYPOINT ["/usr/local/bin/anilist"]
