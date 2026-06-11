#!/usr/bin/env python3
"""Replay generated symbolic-execution test cases."""

import builtins
import contextlib
import io
import json
import runpy
from pathlib import Path


ROOT = Path(__file__).resolve().parent
PROGRAM = ROOT / "original.py"
TEST_CASES = ROOT / "test_cases.json"


def normalize(lines):
    """Compare printed output after trimming trailing whitespace.

    This keeps comparisons exact for line order and values, while avoiding
    failures caused only by trailing spaces in print output.
    """
    return [str(line).rstrip() for line in lines]


def run_program(input_values):
    values = iter(input_values)
    original_input = builtins.input

    def generated_input(prompt=""):
        try:
            return int(next(values))
        except StopIteration as exc:
            raise RuntimeError("Program requested more input values than the test provides") from exc

    stdout = io.StringIO()
    builtins.input = generated_input
    try:
        with contextlib.redirect_stdout(stdout):
            runpy.run_path(str(PROGRAM), run_name="__main__")
    finally:
        builtins.input = original_input

    return stdout.getvalue().splitlines()


def main():
    data = json.loads(TEST_CASES.read_text(encoding="utf-8"))
    tests = data.get("test_cases", [])
    passed = 0

    for test in tests:
        actual = run_program(test.get("input_values", []))
        expected = test.get("expected_output", [])
        ok = normalize(actual) == normalize(expected)
        status = "PASS" if ok else "FAIL"
        print(f"[{status}] Test Case {test.get('id')}")
        if not ok:
            print(f"  inputs:   {test.get('inputs', {})}")
            print(f"  expected: {expected}")
            print(f"  actual:   {actual}")
            print(f"  path:     {test.get('path_condition', '')}")
        passed += 1 if ok else 0

    print(f"\nSummary: {passed}/{len(tests)} tests passed")
    raise SystemExit(0 if passed == len(tests) else 1)


if __name__ == "__main__":
    main()
