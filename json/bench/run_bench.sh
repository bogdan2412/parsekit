#!/bin/sh

set -eu

cd "$(dirname "$0")"
date
exec dune exec ./run_bench.exe -- "${@}"
