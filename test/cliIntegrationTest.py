import http.server
import json
import os
import pathlib
import subprocess
import sys
import threading
import unittest


SERVER_POLL_INTERVAL_SECONDS = 0.01
PROCESS_TIMEOUT_SECONDS = 5
EXECUTABLE = str(pathlib.Path(sys.argv.pop(1)).resolve())


class CliIntegrationTest(unittest.TestCase):
    def setUp(self):
        self.requests = []
        self.response = {"data": {"ok": True}}
        self.status = 200
        owner = self

        class Handler(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
                body = self.rfile.read(int(self.headers["Content-Length"]))

                owner.requests.append(
                    (
                        json.loads(body),
                        {name.lower(): value for name, value in self.headers.items()},
                    )
                )

                response = json.dumps(owner.response).encode()

                self.send_response(owner.status)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(response)))
                self.end_headers()
                self.wfile.write(response)

            def log_message(self, *_arguments):
                pass

        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.thread = threading.Thread(
            target=lambda: self.server.serve_forever(
                poll_interval=SERVER_POLL_INTERVAL_SECONDS
            )
        )

        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()

    def run_cli(self, *arguments):
        environment = dict(
            os.environ,
            ANILIST_GRAPHQL_ENDPOINT=f"http://127.0.0.1:{self.server.server_port}",
        )

        return subprocess.run(
            [EXECUTABLE, *arguments],
            env=environment,
            capture_output=True,
            text=True,
            timeout=PROCESS_TIMEOUT_SECONDS,
        )

    def assert_query(self, arguments, expected):
        result = self.run_cli(*arguments)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.requests[0][0]["query"], expected)

    def test_request_and_response(self):
        result = self.run_cli(
            "--header", "X-Test: value", "media", "--id", "1", "--fields", "id"
        )

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        self.assertEqual(json.loads(result.stdout), self.response)
        self.assertEqual(
            self.requests[0][0], {"query": "query {\n  Media(id: 1) {\n    id\n  }\n}"}
        )
        self.assertEqual(self.requests[0][1]["x-test"], "value")

    def test_nested_field(self):
        self.assert_query(
            [
                "query",
                "--field",
                "media",
                "--id",
                "1",
                "--field",
                "./title",
                "--selection-set",
                "romaji",
            ],
            "query {\n  Media(id: 1) {\n    title {\n      romaji\n    }\n  }\n}",
        )

    def test_nested_fragment(self):
        self.assert_query(
            [
                "query",
                "--field",
                "media",
                "--inline-fragment",
                "Media",
                "--selection-set",
                "id",
            ],
            "query {\n  Media {\n    ... on Media {\n      id\n    }\n  }\n}",
        )

    def test_independent_selections_survive(self):
        self.assert_query(
            [
                "query",
                "--field",
                "__typename",
                "--field",
                "media",
                "--selection-set",
                "id",
                "--field",
                "./title",
                "--selection-set",
                "romaji",
            ],
            "query {\n  __typename\n  Media {\n    id\n  }\n  Media {\n    title {\n      romaji\n    }\n  }\n}",
        )

    def test_inline_fragment_syntax_agrees(self):
        self.run_cli(
            "query", "--inline-fragment", "Query", "--selection-set", "viewer.id"
        )
        self.run_cli("query", "--selection-set", "...on:Query.viewer.id")
        self.assertEqual(self.requests[0][0], self.requests[1][0])

    def test_empty_selection_is_local_error(self):
        result = self.run_cli("query", "--selection-set", "")

        self.assertEqual(result.returncode, 1)
        self.assertEqual(self.requests, [])

    def test_empty_spread_is_local_error(self):
        result = self.run_cli("query", "--selection-set", "...")

        self.assertEqual(result.returncode, 1)
        self.assertEqual(self.requests, [])

    def test_missing_value_is_local_error(self):
        result = self.run_cli("media", "--id", "--selection-set", "id")

        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing value", result.stderr)
        self.assertEqual(self.requests, [])

    def test_trailing_help(self):
        result = self.run_cli("media", "--id", "1", "--help")

        self.assertEqual(result.returncode, 0)
        self.assertIn("Usage:", result.stdout)
        self.assertEqual(result.stderr, "")
        self.assertEqual(self.requests, [])

    def test_invalid_json_is_local_error(self):
        result = self.run_cli("media", "--id", "json:broken", "--selection-set", "id")

        self.assertEqual(result.returncode, 1)
        self.assertIn("Invalid JSON", result.stderr)
        self.assertNotIn("Request failed", result.stderr)
        self.assertEqual(self.requests, [])

    def test_schema_preserves_graphql_error(self):
        self.response = {"data": None, "errors": [{"message": "Upstream diagnostic"}]}
        result = self.run_cli("schema", "--directive", "include")

        self.assertEqual(result.returncode, 0)
        self.assertEqual(json.loads(result.stdout), self.response)
        self.assertEqual(result.stderr, "")

    def test_http_failure(self):
        self.status = 503
        self.response = {"errors": [{"message": "Unavailable"}]}
        result = self.run_cli("media", "--selection-set", "id")

        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, "")
        self.assertEqual(json.loads(result.stderr), self.response)

    def test_equals_value_and_variables(self):
        result = self.run_cli(
            "query",
            "--variable-definition",
            "$name: String!",
            "--variable",
            "name=string:--value",
            "--field",
            "user",
            "--name=--value",
            "--selection-set",
            "id",
        )

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.requests[0][0]["variables"], {"name": "--value"})
        self.assertIn('name: "--value"', self.requests[0][0]["query"])

    def test_empty_scopes_and_fragments(self):
        for expression in [", ,", "root:", "/", "...on:Media."]:
            with self.subTest(expression=expression):
                result = self.run_cli("query", "--selection-set", expression)

                self.assertEqual(result.returncode, 1)
                self.assertEqual(self.requests, [])

    def test_unfinished_inline_fragment_is_local_error(self):
        result = self.run_cli("query", "--inline-fragment", "Query")

        self.assertEqual(result.returncode, 1)
        self.assertEqual(self.requests, [])

    def test_operation_and_fragment_order(self):
        result = self.run_cli(
            "--operation",
            "query:First",
            "--variable",
            "id=1",
            "--field",
            "viewer",
            "--selection-set",
            "id",
            "--fragment",
            "core:Media",
            "--selection-set",
            "id",
            "--operation",
            "query:Second",
            "--variable",
            "id=2",
            "--field",
            "media",
            "--selection-set",
            "...core",
            "--fragment",
            "extra:Media",
            "--selection-set",
            "title.romaji",
            "--execute-operation-name",
            "Second",
        )

        self.assertEqual(result.returncode, 0, result.stderr)

        request = self.requests[0][0]
        query = request["query"]

        self.assertEqual(request["operationName"], "Second")
        self.assertEqual(request["variables"], {"id": 2})
        self.assertLess(query.index("query First"), query.index("query Second"))
        self.assertLess(query.index("fragment core"), query.index("fragment extra"))

    def test_fragment_target_rejects_operation_options(self):
        result = self.run_cli(
            "query",
            "--selection-set",
            "viewer.id",
            "--fragment",
            "core:Media",
            "--variable",
            "id=1",
        )

        self.assertEqual(result.returncode, 1)
        self.assertIn("requires a current operation", result.stderr)
        self.assertEqual(self.requests, [])

    def test_schema_filters_only_directives(self):
        self.response = {
            "data": {
                "__schema": {
                    "directives": [{"name": "include"}, {"name": "skip"}],
                    "extra": 1,
                }
            },
            "extensions": {"trace": "kept"},
        }
        result = self.run_cli("schema", "--directive", "include")
        expected = {
            "data": {"__schema": {"directives": [{"name": "include"}], "extra": 1}},
            "extensions": {"trace": "kept"},
        }

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), expected)

    def test_unrecognized_schema_response_survives(self):
        self.response = {"notice": "No schema available"}
        result = self.run_cli("schema", "--directive", "include")

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), self.response)


if __name__ == "__main__":
    unittest.main()
