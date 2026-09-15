import os
import pathlib
import subprocess
import sys
import tempfile


script = str(pathlib.Path(sys.argv[1]).resolve())

with tempfile.TemporaryDirectory() as directory:
    executable = pathlib.Path(directory) / "docker"

    executable.write_text("#!/bin/sh\nprintf '%s\\n' \"$@\"\n")
    executable.chmod(0o755)

    environment = dict(os.environ, PATH=directory + os.pathsep + os.environ["PATH"])

    for arguments, command, publishes in [
        ([], "build", False),
        (["--push"], "buildx", True),
    ]:
        result = subprocess.run(
            ["sh", script, *arguments],
            env=environment,
            capture_output=True,
            text=True,
            check=True,
        )
        tokens = result.stdout.splitlines()

        assert tokens[0] == command, tokens
        assert ("--push" in tokens) == publishes, tokens

    result = subprocess.run(
        ["sh", script, "--invalid"], env=environment, capture_output=True, text=True
    )

    assert result.returncode == 2, result
    assert result.stdout == "", result
