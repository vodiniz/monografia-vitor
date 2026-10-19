#!/usr/bin/env python3
"""Replay generated tests in isolated processes with a time and step budget."""

import argparse
import builtins
import contextlib
import io
import json
import runpy
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
PROGRAM = ROOT / "test_09_or_condition_original.py"
TEST_CASES = ROOT / "test_09_or_condition_test_cases.json"


def run_program(input_values, step_budget):
    """Preserve the prototype's integer-valued input() convention."""
    values = iter(input_values)
    original_input = builtins.input
    previous_trace = sys.gettrace()
    steps = 0

    def generated_input(prompt=""):
        try:
            return int(next(values))
        except StopIteration as exc:
            raise RuntimeError("Program requested more inputs than the test provides") from exc

    def trace_source(frame, event, arg):
        nonlocal steps
        if event == "line" and frame.f_code.co_filename == str(PROGRAM):
            steps += 1
            if steps > step_budget:
                raise RuntimeError("Source replay exhausted its step budget")
        return trace_source

    stdout = io.StringIO()
    builtins.input = generated_input
    try:
        sys.settrace(trace_source)
        with contextlib.redirect_stdout(stdout):
            runpy.run_path(str(PROGRAM), run_name="__main__")
    finally:
        sys.settrace(previous_trace)
        builtins.input = original_input
    if list(values):
        raise RuntimeError("Program did not consume all generated inputs")
    return stdout.getvalue().splitlines()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--timeout", type=float, default=5.0)
    parser.add_argument("--step-budget", type=int, default=100000)
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    args = parser.parse_args()
    if args.timeout <= 0 or args.step_budget <= 0:
        parser.error("Timeout and step budget must be positive")
    if args.worker:
        values = json.load(sys.stdin)
        print(json.dumps(run_program(values, args.step_budget)))
        return

    tests = json.loads(TEST_CASES.read_text(encoding="utf-8")).get("test_cases", [])
    if not tests:
        print("No validated completed test cases are available.")
        raise SystemExit(2)
    passed = 0
    for test in tests:
        try:
            process = subprocess.run(
                [sys.executable, str(Path(__file__).resolve()), "--worker",
                 "--step-budget", str(args.step_budget)],
                input=json.dumps(test.get("input_values", [])),
                text=True, capture_output=True, timeout=args.timeout, check=True,
            )
            actual = json.loads(process.stdout)
            expected = test.get("expected_output", [])
            # Preserve exact line order and values; ignore only trailing spaces.
            ok = [str(x).rstrip() for x in actual] == [str(x).rstrip() for x in expected]
            print(f"[{'PASS' if ok else 'FAIL'}] Test Case {test.get('id')}")
            if not ok:
                print(f"  inputs: {test.get('input_values', [])}")
                print(f"  expected: {expected}; actual: {actual}")
            passed += int(ok)
        except subprocess.TimeoutExpired:
            print(f"[FAIL] Test Case {test.get('id')}: replay timeout")
        except (subprocess.CalledProcessError, ValueError) as exc:
            detail = exc.stderr.strip() if isinstance(exc, subprocess.CalledProcessError) else str(exc)
            print(f"[FAIL] Test Case {test.get('id')}: {detail}")
    print(f"\nSummary: {passed}/{len(tests)} tests passed")
    raise SystemExit(0 if passed == len(tests) else 1)


if __name__ == "__main__":
    main()
