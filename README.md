# 🍡 `anilist`

> Command-line client for the AniList GraphQL API

`anilist` is an OCaml command-line client for the AniList GraphQL API. It lowers concise CLI arguments into GraphQL operations and prints JSON responses.

<p align="center">
  <img src="assets/screenshot.png" alt="Screenshot">
</p>

## Features

- Ergonomic lowering for common AniList queries
- Tree-shaped selection support with `--field` and `--selection-set`
- Variables, fragments, inline fragments, and multi-operation documents
- Schema introspection via `schema`, `schema --type`, and `schema --directive`
- Custom HTTP headers with repeated `--header`
- Docker runner for use without a local OCaml toolchain

## Build

Use OCaml 5.0 or later, Dune 3.22 or later, and the dependencies declared in
`anilist.opam`. Install dependencies into your opam switch:

```bash
opam install . --deps-only --with-test
```

The declared `lwt_ssl` dependency provides an OpenSSL HTTPS backend. Development
validation also requires Python 3 for the local HTTP integration tests.

```bash
dune build
```

## Install

Install the executable into your local opam prefix:

```bash
dune install --release
```

Then run:

```bash
anilist media --id 1 --selection-set id,title.romaji,format,status
```

## Docker

Build the image:

```bash
docker build -t fuwn/anilist .
```

Run a query:

```bash
docker run --rm fuwn/anilist media --id 1 --selection-set id,title.romaji,format,status
```

The build runs the OCaml and local HTTP integration tests. The build script
creates a local image by default:

```bash
scripts/build-docker-image.sh
```

Publication is explicit: `scripts/build-docker-image.sh --push` publishes
`latest` and a UTC date tag to `IMAGE_NAME` (default `fuwn/anilist`). It builds
`PLATFORMS` (default `linux/amd64,linux/arm64`).

## Development

Run `dune build`, `dune runtest`, and `git diff --check` before committing.

## Shell Script Savings

In a shell script, the usual GraphQL flow means carrying an endpoint, a JSON payload, and a quoted query string:

```bash
endpoint='https://graphql.anilist.co'

curl \
  --silent \
  --header 'Content-Type: application/json' \
  --data '{"query":"query { Media(id: 1) { id title { romaji } format status } }"}' \
  "$endpoint"
```

With `anilist`, the same request becomes the command itself:

```bash
anilist media --id 1 --selection-set id,title.romaji,format,status
```

That removes the JSON wrapper, the embedded GraphQL string, and most of the shell quoting friction while keeping the JSON response unchanged.

## Usage

Get an anime:

```bash
anilist media --id 1 --selection-set id,title.romaji,format,status
```

Inspect the schema:

```bash
anilist schema
anilist schema --type Media
anilist schema --directive include
```

Send an authorization header:

```bash
anilist --header 'Authorization: Bearer <token>' media --id 1 --selection-set id,title.romaji
```

Use variables explicitly:

```bash
anilist query \
  --variable-definition '$media_id: Int!' \
  --variable 'media_id=1' \
  --field media \
  --id var:media_id \
  --selection-set id,title.romaji,format,status
```

The endpoint defaults to `https://graphql.anilist.co`. Override it with `ANILIST_GRAPHQL_ENDPOINT`:

```bash
ANILIST_GRAPHQL_ENDPOINT='https://graphql.anilist.co' anilist media --id 1 --selection-set id,title.romaji
```

## Request Results

HTTP 2xx responses print to stdout and exit 0; other HTTP statuses print to stderr
and exit 1. GraphQL `errors` inside an HTTP 2xx response remain in the JSON output
and do not change the exit status. Scripts should inspect the response when they
need GraphQL-level success detection.

Requests have a 30-second deadline, including the response body. Override it with
`ANILIST_REQUEST_TIMEOUT_SECONDS`, using a positive finite number of seconds.

## License

Licensed under the GNU Affero General Public License v3.0 only. See [LICENSE](./LICENSE).
