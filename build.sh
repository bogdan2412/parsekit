#!/bin/sh

set -eu

cd "$(dirname "$0")"
dune build @fmt --auto-promote || true
dune build @runtest @install
