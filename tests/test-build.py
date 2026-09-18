#!/usr/bin/env python3
"""Exercise build entry points and upgrade packaging in an isolated checkout."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]


def run(*args, cwd, env=None):
    result = subprocess.run(args, cwd=cwd, env=env, text=True, capture_output=True, timeout=120)
    if result.returncode:
        raise AssertionError(f"{args!r} failed:\n{result.stdout}\n{result.stderr}")
    return result


class BuildTests(unittest.TestCase):
    def test_build_paths_and_upgrade_inputs(self):
        with tempfile.TemporaryDirectory(prefix="zkeme80-build-") as temporary:
            workspace = Path(temporary) / "checkout with spaces"
            workspace.mkdir()
            shutil.copytree(
                ROOT / "src", workspace / "src",
                ignore=shutil.ignore_patterns("*.rom", "*.8xu", "*.json", "*.go"),
            )
            for name in ("Makefile", "build.scm", "0A.key"):
                shutil.copy2(ROOT / name, workspace / name)

            # A checkout may already contain a directory named build.
            (workspace / "build").mkdir()
            run("make", "build", cwd=workspace)
            expected = {}
            for name in ("zkeme80.rom", "zkeme80.ram-labelmap.json"):
                expected[name] = (workspace / name).read_bytes()
                self.assertEqual(expected[name], (workspace / "src" / name).read_bytes())
            self.assertEqual(len(expected["zkeme80.rom"]), 1024 * 1024)
            self.assertIsInstance(json.loads(expected["zkeme80.ram-labelmap.json"]), dict)

            # Includes must resolve next to build.scm, while outputs belong to
            # the caller even when the caller is outside the source tree.
            output = Path(temporary) / "external output"
            output.mkdir()
            run("guile", "--no-auto-compile", str(workspace / "build.scm"), cwd=output)
            for name, data in expected.items():
                self.assertEqual((output / name).read_bytes(), data)

            # mktiupgrade is optional. Replace only the packager, checking its
            # real ROM/key inputs and page selection before writing a marker.
            tools = Path(temporary) / "tools"
            tools.mkdir()
            packager = tools / "mktiupgrade"
            packager.write_text('''#!/usr/bin/env python3
from pathlib import Path
import sys
args = sys.argv[1:]
assert args[:4] == ["-k", "../0A.key", "--device", "TI-84+"]
assert Path(args[1]).is_file()
rom = Path(args[4]).read_bytes()
assert len(rom) == 1024 * 1024
pages = [int(page, 16) for page in args[6:]]
assert pages == [0, 1, 2, 3, 4, 5, 60]
for page in pages:
    assert rom[page * 16384:(page + 1) * 16384] != bytes([255]) * 16384
Path(args[5]).write_bytes(b"verified upgrade inputs")
''')
            packager.chmod(0o755)
            environment = dict(os.environ, PATH=f"{tools}{os.pathsep}{os.environ['PATH']}")
            run("make", "upgrade", "GUILE_BUILD=(exit 0)", cwd=workspace, env=environment)
            self.assertEqual((workspace / "zkeme80.8xu").read_bytes(), b"verified upgrade inputs")
            self.assertEqual(
                (workspace / "src" / "zkeme80.8xu").read_bytes(),
                (workspace / "zkeme80.8xu").read_bytes(),
            )


if __name__ == "__main__":
    unittest.main()
