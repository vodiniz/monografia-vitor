#!/usr/bin/env bash
set -euo pipefail

# Run from the project root so relative example paths match the documented CLI.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
EXAMPLES_DIR="examples/python"

cd "${PROJECT_ROOT}"

shopt -s nullglob

for example in "${EXAMPLES_DIR}"/*.py; do
    # The runner is a shell script, but keep this guard if the pattern changes.
    if [[ "$(basename "${example}")" == "run_all_examples.sh" ]]; then
        continue
    fi

    echo "========================================"
    echo "Running ${example}"
    echo "========================================"
    cabal run symbolic -- "${example}"
done
